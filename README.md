![WIP](https://img.shields.io/badge/status-work_in_progress-orange?style=plastic)
![Ada](https://img.shields.io/badge/Language-Ada-022881?style=plastic&logo=ada&logoColor=white)
![FreeBSD](https://img.shields.io/badge/OS-FreeBSD-AB2B28?style=plastic&logo=freebsd&logoColor=white)

# parchemin
Parchemin is a robust command-line tool designed to generate structured data files, s
uch as Intel HEX or Motorola S-Record formats. It ensures high precision and built-in 
error checking for populating non-volatile memory during system initialization.

Unlike standard conversion tools, Parchemin acts as a bridge between data definition
and firmware implementation by providing a clear memory layout plan, helping developers
align their code with the exact physical placement of variables.

[!WARNING]
**Work in progress**: There is no functionakl release of Parchemin available yet.
Watch the project to be notified Version 1.0 is ready.

## Key Features
- **Multi-Format Generation**: Native support for industry-standard formats, 
including Intel HEX and Motorola S-Record (S19, S28, S37).

- **Data Integrity & Safety**: Automatic checksum calculation and record-level 
validation to prevent memory corruption, leveraging Ada’s native reliability.

- **Intelligent Memory Mapping**: Automatically generates a detailed Memory Layout Plan. 
It maps constants and variables to their specific hardware addresses, providing a clear 
blueprint for the developer.

- **Firmware-Data Alignment**: Simplifies the synchronization between low-level firmware code 

- **High Precision Control**: Fine-grained management of padding, byte alignment, and address 
offsets for all types of non-volatile storage (Flash, EEPROM, etc.).

## How to Build
` gprbuild parchemin.gpr`

## First release roadmap
- [x] MCU specification loader.
- [x] Abstract type for command line implémentation.
- [x] List command for mcu specs
- [ ] Encoder
*No way ! a lot of new taks could be added here ... i'm serious*
