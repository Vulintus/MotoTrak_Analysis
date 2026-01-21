# MotoTrak Analysis

A comprehensive MATLAB-based suite for analyzing behavioral data from MotoTrak systems. This toolbox provides data visualization, file conversion, session tracking, and daily reporting capabilities for neuroscience and behavioral research using MotoTrak devices.

_Disclaimer: This README file was AI-generated, but checked for accuracy by a human._

## Overview

MotoTrak Analysis is designed to process and analyze data from MotoTrak behavioral training systems. It supports multiple device types (knob, lever, pull) and provides tools for data export, visualization, session editing, and daily performance reporting.

**Usage Options:**
- **Standalone Application**: Install via `mototrak_analysis_v1p20_installer_win64.exe` (no MATLAB required)
- **MATLAB Source**: Run directly from MATLAB using the collated `MotoTrak_Analysis.m` script

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

### Two Ways to Use MotoTrak Analysis

#### Option 1: Compiled Executable (Recommended for Most Users)
For users **without MATLAB**, install the standalone application:

1. Download and run `mototrak_analysis_v1p20_installer_win64.exe` from the [`compiled/installers/`](compiled/installers/) directory
2. Follow the installation wizard (MATLAB Runtime will be installed automatically if needed)
3. Launch "MotoTrak Analysis" from your Start Menu or desktop shortcut

**System Requirements:**
- Windows 10 or later (64-bit)
- ~1 GB disk space for MATLAB Runtime
- See `compiled/redistribution_files_only/readme.txt` for details

#### Option 2: MATLAB Source Code
For users **with MATLAB** who want to modify or extend functionality:

1. Clone or download this repository
2. Add the repository folder to your MATLAB path
3. Run `MotoTrak_Analysis` in MATLAB to launch the main interface

**Requirements:**
- MATLAB R2019b or later (recommended)
- Required MATLAB toolboxes are listed in [src/Required Toolbox Functions/required_matlab_products.txt](src/Required Toolbox Functions/required_matlab_products.txt)

**Note:** All MATLAB functions are collated into the single script `MotoTrak_Analysis.m` for easy deployment and execution.

## Usage

### Quick Start
```matlab
% Launch the main analysis interface
MotoTrak_Analysis

% Or run specific tools directly:
MotoTrak_Graphical_Analysis    % Interactive data visualization
MotoTrak_Daily_Report          % Generate daily summary reports
MotoTrak_File_Editor          % Edit file metadata
MotoTrak_SessionData_to_TSV   % Export session data to TSV
```

### Configuration
On first launch, the program creates a configuration file at:
- Windows: `%LOCALAPPDATA%\Vulintus\MotoTrak Analysis\`

You can customize default paths and settings through the configuration interface or by editing the config file directly.

## Data File Formats

This toolbox supports:
- **`.MotoTrak`** - Current MotoTrak data format
- **`.ArdyMotor`** - Legacy ArdyMotor format (backwards compatible)

## Project Structure

```
MotoTrak_Analysis/
├── MotoTrak_Analysis.m           # Main entry point
├── src/                          # Source code
│   ├── MotoTrak_Graphical_Analysis.m
│   ├── MotoTrak_Daily_Report.m
│   ├── MotoTrak_File_Editor.m
│   ├── MotoTrak_SessionData_to_TSV.m
│   ├── MotoTrak_*_Viewer.m       # Device-specific viewers
│   └── Required Toolbox Functions/
├── compiled/                     # Compiled standalone version
└── README.md
```

## Development

### Key Functions
- **File Reading**: `MotoTrakFileRead.m`, `ArdyMotorFileRead.m`, `MotoTrakHeaderRead.m`
- **Configuration**: `MotoTrak_Analysis_Default_Config.m`, `MotoTrak_Analysis_Edit_Config.m`
- **Utilities**: Located in `src/Required Toolbox Functions/`

### Building Compiled Version
Use `src/Deploy_MotoTrak_Analysis.m` to create a standalone executable via MATLAB Compiler.

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
