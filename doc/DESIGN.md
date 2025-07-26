// placeholder
# DeepSick System Architecture

## Overview
DeepSick is a multi-layered destruction system with:
- **Bootkit**: Low-level disk manipulation
- **Kernel Module**: Filesystem destruction
- **User Dropper**: Stealth deployment

```mermaid
graph TD
    A[Bootkit] -->|MBR Infection| B(Kernel Loader)
    B --> C[Kernel Module]
    D[User Dropper] -->|memfd_create| C
    C --> E[NTFS/EXT4 Destruction]
    C --> F[Anti-Forensics]