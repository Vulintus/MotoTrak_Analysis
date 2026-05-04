# MotoTrak Analysis

A MATLAB and Python toolkit for loading and analyzing behavioral data from MotoTrak systems. This repository includes GUI analysis tools, file conversion utilities, and standalone data-loader scripts you can call from your own custom code.

_Disclaimer: This README file was AI-generated, but checked for accuracy by a human._

## Overview

MotoTrak Analysis is designed to process MotoTrak behavioral training data for devices such as pull, knob, and lever modules. You can use it in two common ways:

- Run the MATLAB GUI/analysis workflow
- Call the reader scripts directly from your own MATLAB or Python analysis pipeline

## File-Reading Scripts For Custom Code

If you are writing your own analysis scripts, start with these readers:

- MATLAB: [MATLAB/ArdyMotorFileRead.m](MATLAB/ArdyMotorFileRead.m)
- MATLAB: [MATLAB/MotoTrakFileRead.m](MATLAB/MotoTrakFileRead.m)
- Python: [Python/ArdyMotorFileRead.py](Python/ArdyMotorFileRead.py)
- Python: [Python/MotoTrakFileRead.py](Python/MotoTrakFileRead.py)

Binary format details are documented in:

- [MotoTrak_File_Structure.md](MotoTrak_File_Structure.md)

### MATLAB Reader Usage

#### 1) Read legacy `.ArdyMotor` files

```matlab
% Returns data struct and file format version
[data, version] = ArdyMotorFileRead('D:\Data\Animal01\session01.ArdyMotor');

% Example custom analysis
n_trials = numel(data.trial);
hit_trials = sum(arrayfun(@(t) strcmpi(char(t.outcome), 'H'), data.trial));
fprintf('Trials: %d, Hits: %d\n', n_trials, hit_trials);
```

#### 2) Read `.MotoTrak` / `.MOTOTRAK` files

```matlab
session = MotoTrakFileRead('D:\Data\Animal01\session02.MotoTrak');

% Example custom analysis
trial_count = numel(session.trial);
mean_hits_per_trial = mean(arrayfun(@(t) numel(t.hit_times), session.trial));
fprintf('Trials: %d, Mean hits/trial: %.2f\n', trial_count, mean_hits_per_trial);
```

### Python Reader Usage

You can import the Python reader modules directly in your scripts.

#### 1) Read legacy `.ArdyMotor` files

```python
from ArdyMotorFileRead import ArdyMotorFileRead

data, version = ArdyMotorFileRead(r"D:\Data\Animal01\session01.ArdyMotor")
print("version:", version)
print("subject:", data.get("subject"))
print("trials:", len(data.get("trial", [])))
```

#### 2) Read `.MotoTrak` / `.MOTOTRAK` files

```python
from MotoTrakFileRead import MotoTrakFileRead

session = MotoTrakFileRead(r"D:\Data\Animal01\session02.MotoTrak")
print("version:", session.get("version"))
print("subject:", session.get("subject"))
print("trials:", len(session.get("trial", [])))
```

#### 3) Run the included example script

See [Python/example_usage.py](Python/example_usage.py):

```bash
python Python/example_usage.py --ardymotor "D:/Data/session01.ArdyMotor"
python Python/example_usage.py --mototrak "D:/Data/session02.MotoTrak"
python Python/example_usage.py --ardymotor "D:/Data/a.ArdyMotor" --mototrak "D:/Data/b.MotoTrak"
```

## Data File Formats

Supported session-file formats:

- `.MotoTrak` / `.MOTOTRAK` (MotoTrak 2.x data files)
- `.ArdyMotor` (legacy format)

## Features

### Core Analysis Tools
- **Graphical Analysis** - Interactive visualization of behavioral session data across multiple files
- **Session Trace** - Detailed trace viewing of individual training sessions
- **Daily Reports** - Generate comprehensive daily performance summaries across animals
- **File Editor** - Edit metadata (subject, booth, stage) in existing MotoTrak files

### Device-Specific Viewers
- **Knob Viewer** - Analyze rotational knob task data
- **Lever Viewer** - Examine lever press task performance
- **Pull Viewer** - Review pull task sessions

### Data Conversion
- **Session Data to TSV** - Export session data to tab-separated values for Excel/statistical software
- **PopData to TSV** - Convert population data to tabular format
- **ArdyMotor Format Conversion** - Convert between legacy ArdyMotor and MotoTrak file formats

## Installation

### Two Ways To Use MotoTrak Analysis

#### Option 1: Compiled Executable (Recommended for most users)
For users without MATLAB, install the standalone application:

1. Download and run `mototrak_analysis_v1p20_installer_win64.exe` from [compiled/installers/](compiled/installers/)
2. Follow the installation wizard (MATLAB Runtime will be installed automatically if needed)
3. Launch "MotoTrak Analysis" from your Start Menu or desktop shortcut

**System Requirements:**
- Windows 10 or later (64-bit)
- ~1 GB disk space for MATLAB Runtime
- See [compiled/redistribution_files_only/readme.txt](compiled/redistribution_files_only/readme.txt) for details

#### Option 2: MATLAB source code
For users with MATLAB who want to modify or extend functionality:

1. Clone or download this repository
2. Add the repository folder to your MATLAB path
3. Run `MotoTrak_Analysis` in MATLAB to launch the main interface

**Requirements:**
- MATLAB R2019b or later (recommended)
- Required MATLAB toolboxes are listed in [MATLAB/src/Required Toolbox Functions/required_matlab_products.txt](MATLAB/src/Required Toolbox Functions/required_matlab_products.txt)

Note: the collated entry-point script is [MATLAB/MotoTrak_Analysis.m](MATLAB/MotoTrak_Analysis.m).

## Usage

### Quick Start
```matlab
% Launch the main analysis interface
MotoTrak_Analysis

% Or run specific tools directly:
MotoTrak_Graphical_Analysis   % Interactive data visualization
MotoTrak_Daily_Report         % Generate daily summary reports
MotoTrak_File_Editor          % Edit file metadata
MotoTrak_SessionData_to_TSV   % Export session data to TSV
```

### Configuration
On first launch, the program creates a configuration file at:
- Windows: `%LOCALAPPDATA%\Vulintus\MotoTrak Analysis\`

You can customize default paths and settings through the configuration interface or by editing the config file directly.

## Project Structure

```
MotoTrak_Analysis/
├── MATLAB/
│   ├── MotoTrak_Analysis.m       # Main MATLAB entry point
│   ├── ArdyMotorFileRead.m       # MATLAB legacy-file reader
│   ├── MotoTrakFileRead.m        # MATLAB MotoTrak-file reader
│   └── src/                      # MATLAB analysis modules
├── Python/
│   ├── ArdyMotorFileRead.py      # Python legacy-file reader
│   ├── MotoTrakFileRead.py       # Python MotoTrak-file reader
│   └── example_usage.py          # Python usage example
├── MotoTrak_File_Structure.md    # Binary-format field layout
├── compiled/                     # Compiled standalone version
└── README.md
```

## Development

### Key Functions
- MATLAB file reading: [MATLAB/MotoTrakFileRead.m](MATLAB/MotoTrakFileRead.m), [MATLAB/ArdyMotorFileRead.m](MATLAB/ArdyMotorFileRead.m)
- Python file reading: [Python/MotoTrakFileRead.py](Python/MotoTrakFileRead.py), [Python/ArdyMotorFileRead.py](Python/ArdyMotorFileRead.py)
- MATLAB utilities and GUI modules: [MATLAB/src/](MATLAB/src/)

### Building Compiled Version
Use [MATLAB/src/Deploy_MotoTrak_Analysis.m](MATLAB/src/Deploy_MotoTrak_Analysis.m) to create a standalone executable via MATLAB Compiler.

## License

Copyright © Vulintus, Inc. All rights reserved.

## Support

For questions or issues, please contact Vulintus, Inc.

## Citation

If you use this toolbox in your research, please cite:
> MotoTrak Analysis Toolbox, Vulintus, Inc., https://github.com/[repository-url]

## Version History

- **2025-06-26** - Latest collation with inter-press interval analysis
- **2019-09-26** - Added lever module press-time outputs
- **2016-08-08** - Combined analysis GUI integration
- **2015-07-06** - Initial TSV export functionality created
