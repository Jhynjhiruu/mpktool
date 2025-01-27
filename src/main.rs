use std::{
    fmt::Display,
    fs::{create_dir_all, read, write},
    path::PathBuf,
    str::FromStr,
};

use anyhow::{anyhow, Result};
use clap::{Parser, Subcommand};
use mpktool::{build, extract, Inode, InodeTable, Note};
use unfmt::unformat;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[clap(subcommand)]
    command: Command,

    /// Input path
    inpath: PathBuf,

    /// Output path
    outpath: PathBuf,
}

#[derive(Subcommand)]
enum Command {
    /// Extract notes from a controller pak file
    Extract,

    /// Build a controller pak file from notes
    Build,
}

enum FileOrFolder {
    File,
    Folder,
}

impl Display for FileOrFolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::File => "file",
                Self::Folder => "folder",
            }
        )
    }
}

impl Command {
    fn which(&self) -> (FileOrFolder, FileOrFolder) {
        match self {
            Self::Extract => (FileOrFolder::File, FileOrFolder::Folder),
            Self::Build => (FileOrFolder::Folder, FileOrFolder::File),
        }
    }
}

fn main() -> Result<()> {
    let args = Args::parse();

    let (in_type, out_type) = args.command.which();

    if !args.inpath.exists() {
        return Err(anyhow!(
            "input path {} does not exist",
            args.inpath.display()
        ));
    }

    if match in_type {
        FileOrFolder::File => !args.inpath.is_file(),
        FileOrFolder::Folder => !args.inpath.is_dir(),
    } {
        return Err(anyhow!(
            "input path {} is not a {}",
            args.inpath.display(),
            in_type,
        ));
    }

    if args.outpath.exists()
        && match out_type {
            FileOrFolder::File => !args.outpath.is_file(),
            FileOrFolder::Folder => !args.outpath.is_dir(),
        }
    {
        return Err(anyhow!(
            "output path {} is not a {}",
            args.outpath.display(),
            out_type,
        ));
    }

    match args.command {
        Command::Extract => {
            let infile = read(args.inpath)?;

            create_dir_all(&args.outpath)?;

            let notes = extract(&infile)?;

            for (note, data) in notes {
                let filename = note.to_string();

                write(args.outpath.join(filename), data.concat())?;
            }
        }
        Command::Build => {
            let mut notes = vec![];

            for file in args.inpath.read_dir()? {
                let file = file?;

                let filename = match file.file_name().into_string() {
                    Ok(s) => s,
                    Err(o) => o.to_string_lossy().to_string(),
                };

                if !file.metadata()?.is_file() {
                    continue;
                }

                let data = read(file.path())?;

                let note = Note::from_str(&filename)?;

                notes.push((note, data))
            }

            let built = build(&notes)?;

            if let Some(p) = args.outpath.parent() {
                create_dir_all(p)?;
            }

            write(args.outpath, built)?;
        }
    }

    Ok(())
}
