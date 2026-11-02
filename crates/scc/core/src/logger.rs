use std::path::{Path, PathBuf};

use chrono::SecondsFormat;
use flexi_logger::{
    Age, Cleanup, Criterion, Duplicate, FileSpec, FlexiLoggerError, LogSpecBuilder, Logger, Naming,
};
use log::LevelFilter;

pub fn log_dir(root_dir: &Path) -> PathBuf {
    root_dir.join("r6").join("logs")
}

pub fn setup(root_dir: &Path) {
    let directory = log_dir(root_dir);
    let file = FileSpec::default()
        .directory(&directory)
        .basename("redscript");

    let result = Logger::with(LogSpecBuilder::new().default(LevelFilter::Info).build())
        .log_to_file(file)
        .duplicate_to_stdout(Duplicate::All)
        .rotate(
            Criterion::Age(Age::Day),
            Naming::Timestamps,
            Cleanup::KeepLogFiles(4),
        )
        .format(|out, time, msg| {
            let time = time
                .now()
                .with_timezone(&chrono::Utc)
                .to_rfc3339_opts(SecondsFormat::Secs, true);
            write!(out, "[{} - {time}] {}", msg.level(), msg.args())
        })
        .start();

    match result {
        Ok(_) | Err(FlexiLoggerError::Log(_)) => {}
        Err(err) => {
            eprintln!(
                "Failed to initialize REDscript logging in '{}': {:#}. Falling back to stderr.",
                directory.display(),
                anyhow::Error::new(err)
            );
            match Logger::with(LogSpecBuilder::new().default(LevelFilter::Info).build())
                .log_to_stderr()
                .start()
            {
                Ok(_) | Err(FlexiLoggerError::Log(_)) => {}
                Err(err) => eprintln!(
                    "Failed to initialize REDscript stderr logging: {:#}",
                    anyhow::Error::new(err)
                ),
            }
        }
    }
}
