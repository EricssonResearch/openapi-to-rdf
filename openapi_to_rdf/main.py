import argparse
import logging
import os
import sys
from pathlib import Path

from openapi_to_rdf.shacl_converter import OpenAPIToSHACLConverter
from openapi_to_rdf.rdf_converter import OpenAPIToRDFConverter


def download_command(args):
    """Handle the download subcommand."""
    from openapi_to_rdf.download_3gpp_openapi import ThreeGPPDownloader
    
    downloader = ThreeGPPDownloader(args.output_dir, args.dry_run)
    
    if args.list_releases:
        downloader.list_available_releases()
        return
    
    if not args.release:
        print("Error: --release is required (use --list-releases to see available options)")
        sys.exit(1)
    
    success = downloader.download_release(args.release)
    sys.exit(0 if success else 1)


def convert_command(args):
    """Handle the convert subcommand (default behavior)."""
    input_path = args.input
    base_namespace = args.base_namespace
    output_format = args.format
    namespace_prefix = args.namespace_prefix

    # Validate input path exists
    if not os.path.exists(input_path):
        logging.error(f"Input path does not exist: {input_path}")
        sys.exit(1)

    yaml_files = []
    if os.path.isdir(input_path):
        # Always search recursively by default - find all YAML files in directory and subdirectories
        yaml_files = list(Path(input_path).rglob("*.yaml"))
        yaml_files = [str(f) for f in yaml_files]
        print(f"Found {len(yaml_files)} YAML files in {input_path}")
    elif os.path.isfile(input_path) and input_path.endswith(".yaml"):
        yaml_files.append(input_path)
        print(f"Processing single file: {input_path}")
    else:
        logging.error(
            "Invalid input: must be a YAML file or a directory containing YAML files."
        )
        sys.exit(1)

    if not yaml_files:
        logging.error("No YAML files found to process.")
        sys.exit(1)

    # Process each YAML file
    successful_conversions = 0
    failed_conversions = 0
    
    for i, yaml_file in enumerate(yaml_files, 1):
        print(f"[{i}/{len(yaml_files)}] Converting {yaml_file} to {output_format.upper()}...")
        
        try:
            if output_format == "shacl":
                converter = OpenAPIToSHACLConverter(
                    yaml_file, 
                    base_namespace=base_namespace, 
                    external_refs=[],
                    base_namespace_prefix=namespace_prefix
                )
            else:  # owl
                # Fallback to original namespace format for OWL if not provided
                if base_namespace is None:
                    base_namespace = f"{namespace_prefix}rdf/"
                converter = OpenAPIToRDFConverter(yaml_file, base_namespace, external_refs=[])
            
            converter.run()
            successful_conversions += 1
            print(f"✓ Successfully converted {yaml_file}")
            
        except Exception as e:
            failed_conversions += 1
            print(f"✗ Failed to convert {yaml_file}: {str(e)}")
            logging.error(f"Conversion failed for {yaml_file}: {str(e)}")
    
    # Summary
    print(f"\nConversion Summary:")
    print(f"  Total files processed: {len(yaml_files)}")
    print(f"  Successful conversions: {successful_conversions}")
    print(f"  Failed conversions: {failed_conversions}")
    
    if failed_conversions > 0:
        sys.exit(1)


def main():
    """Main entry point for the openapi-rdf-converter CLI."""
    parser = argparse.ArgumentParser(
        description="OpenAPI RDF Converter - Download 3GPP specifications and convert to RDF/SHACL",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Download 3GPP specifications
  openapi-to-rdf download --release Rel-18 --output-dir assets/
  openapi-to-rdf download --release Rel-19 --dry-run
  
  # Convert OpenAPI files to RDF/SHACL
  openapi-to-rdf convert path/to/openapi.yaml
  openapi-to-rdf convert assets/MnS-Rel-18-OpenAPI/OpenAPI/
  
  # Complete workflow
  openapi-to-rdf download --release Rel-18 --output-dir assets/
  openapi-to-rdf convert assets/MnS-Rel-18-OpenAPI/OpenAPI/ --namespace-prefix "https://myorg.com/models/"
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Download subcommand
    download_parser = subparsers.add_parser('download', help='Download 3GPP OpenAPI specifications')
    download_parser.add_argument(
        "--release",
        help="3GPP release to download (e.g., Rel-18, Rel-19)"
    )
    download_parser.add_argument(
        "--output-dir",
        default="assets",
        help="Output directory for downloaded files (default: assets/)"
    )
    download_parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be downloaded without actually downloading"
    )
    download_parser.add_argument(
        "--list-releases",
        action="store_true",
        help="List available releases and exit"
    )
    
    # Convert subcommand
    convert_parser = subparsers.add_parser('convert', help='Convert OpenAPI YAML to RDF/SHACL')
    convert_parser.add_argument(
        "input", help="Path to a YAML file or a directory containing YAML files."
    )
    convert_parser.add_argument(
        "--base_namespace",
        default=None,
        help="Base namespace for RDF output (auto-generated if not provided)",
    )
    convert_parser.add_argument(
        "--format",
        choices=["shacl", "owl"],
        default="shacl",
        help="Output format: 'shacl' for separate RDF vocabulary + SHACL shapes (default), 'owl' for RDF/OWL"
    )
    convert_parser.add_argument(
        "--namespace-prefix",
        default="http://ericsson.com/models/3gpp/",
        help="Base namespace prefix for generated URIs (default: http://ericsson.com/models/3gpp/)"
    )
    
    # Check for backward compatibility first
    if len(sys.argv) > 1 and sys.argv[1] not in ['download', 'convert', '--help', '-h']:
        # Old syntax - treat as convert command
        convert_args = argparse.Namespace()
        convert_args.input = sys.argv[1]
        convert_args.base_namespace = None
        convert_args.format = 'shacl'
        convert_args.namespace_prefix = 'http://ericsson.com/models/3gpp/'
        
        # Parse additional arguments
        remaining_args = sys.argv[2:] if len(sys.argv) > 2 else []
        i = 0
        while i < len(remaining_args):
            arg = remaining_args[i]
            if arg == '--format' and i + 1 < len(remaining_args):
                convert_args.format = remaining_args[i + 1]
                i += 2
            elif arg == '--namespace-prefix' and i + 1 < len(remaining_args):
                convert_args.namespace_prefix = remaining_args[i + 1]
                i += 2
            elif arg == '--base_namespace' and i + 1 < len(remaining_args):
                convert_args.base_namespace = remaining_args[i + 1]
                i += 2
            else:
                i += 1
        
        convert_command(convert_args)
    else:
        # New subcommand syntax
        args = parser.parse_args()
        
        if args.command == 'download':
            download_command(args)
        elif args.command == 'convert':
            convert_command(args)
        else:
            parser.print_help()
            sys.exit(1)


if __name__ == "__main__":
    main()
