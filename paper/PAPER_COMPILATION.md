# JOSS Paper Compilation

This directory contains the paper submission for the Journal of Open Source Software (JOSS).

## Files

- `paper/paper.md` - Main paper content in Markdown format
- `paper/paper.bib` - Bibliography file with references
- `paper/paper.pdf` - Compiled PDF output (generated)
- `paper/jats/paper.jats` - JATS XML format (generated)

## Compilation

To compile the paper using Docker (as recommended by JOSS):

```bash
docker run --rm --volume $PWD:/data --user $(id -u):$(id -g)  --env JOURNAL=joss  openjournals/inara
```

This command will:
1. Download the JOSS compilation environment (first run only)
2. Generate `paper.pdf` from the Markdown source
3. Generate JATS XML output in `paper/jats/paper.jats`

## Requirements

- Docker installed and running
- Paper files in `paper/` directory
- Internet connection (for Docker image download)

## JOSS Submission

The generated `paper.pdf` can be submitted to JOSS along with the repository URL. The JATS XML format is used by JOSS for final publication.

For more information, see: https://joss.readthedocs.io/en/latest/paper.html