#!/usr/bin/env python3
"""
Download 3GPP OpenAPI specifications from GitLab repository.

This script downloads all OpenAPI YAML files from the 3GPP SA5 MnS repository
for a specific release (e.g., Rel-18, Rel-19).

Usage:
    python download_3gpp_openapi.py --release Rel-18 --output-dir assets/
    python download_3gpp_openapi.py --release Rel-19 --output-dir assets/ --dry-run
"""

import argparse
import os
import requests
import sys
from pathlib import Path
from urllib.parse import urljoin
import re


class ThreeGPPDownloader:
    """Download 3GPP OpenAPI specifications from GitLab."""
    
    REPO_BASE_URL = "https://forge.3gpp.org/rep/sa5/MnS/-/raw"
    
    def __init__(self, output_dir="assets", dry_run=False):
        self.output_dir = Path(output_dir)
        self.dry_run = dry_run
        
    def get_release_branch(self, release):
        """Get the branch name for a specific release."""
        # Map release names to branch names
        release_mapping = {
            "Rel-18": "Rel-18",
            "Rel-19": "Rel-19", 
            "Rel-17": "Rel-17",
            "Rel-16": "Rel-16",
            "Rel-15": "Rel-15",
        }
        
        if release in release_mapping:
            return release_mapping[release]
        
        # If not in mapping, assume it's already a valid branch name
        return release
    
    def get_directory_listing(self, branch, path="OpenAPI"):
        """Get directory listing using multiple discovery methods."""
        print(f"Discovering YAML files in {branch}/{path}...")
        
        # Method 1: Try GitLab API
        api_files = self._try_gitlab_api(branch, path)
        if api_files:
            return api_files
        
        # Method 2: Try web scraping
        web_files = self._try_web_scraping(branch, path)
        if web_files:
            return web_files
        
        # Method 3: Try common patterns
        pattern_files = self._try_pattern_discovery(branch, path)
        if pattern_files:
            return pattern_files
        
        print("All discovery methods failed")
        return []
    
    def _try_gitlab_api(self, branch, path):
        """Try GitLab API method."""
        try:
            api_url = "https://forge.3gpp.org/api/v4/projects/104/repository/tree"
            params = {
                "ref": branch,
                "path": path,
                "recursive": "true"
            }
            
            response = requests.get(api_url, params=params, timeout=30)
            response.raise_for_status()
            
            tree_data = response.json()
            
            yaml_files = []
            for item in tree_data:
                if (item.get("type") == "blob" and 
                    item.get("name", "").endswith(".yaml") and
                    item.get("path", "").startswith(f"{path}/")):
                    filename = item["name"]
                    yaml_files.append(filename)
            
            if yaml_files:
                print(f"✓ GitLab API found {len(yaml_files)} files")
                return yaml_files
            
        except requests.RequestException as e:
            print(f"✗ GitLab API failed: {e}")
        
        return []
    
    def _try_web_scraping(self, branch, path):
        """Try web scraping method."""
        try:
            url = f"https://forge.3gpp.org/rep/sa5/MnS/-/tree/{branch}/{path}"
            response = requests.get(url, timeout=30)
            response.raise_for_status()
            
            import re
            
            # Look for GitLab file browser patterns
            yaml_patterns = [
                r'href="[^"]*/([^/"]+\.yaml)"',
                r'"([^"]+\.yaml)"',
                r'([A-Za-z0-9_]+\.yaml)',
            ]
            
            files = []
            for pattern in yaml_patterns:
                matches = re.findall(pattern, response.text)
                for match in matches:
                    if match not in files and match.endswith('.yaml'):
                        files.append(match)
            
            if files:
                print(f"✓ Web scraping found {len(files)} files")
                return files
            
        except requests.RequestException as e:
            print(f"✗ Web scraping failed: {e}")
        
        return []
    
    def _try_pattern_discovery(self, branch, path):
        """Try discovering files using common 3GPP patterns."""
        print("Trying pattern-based discovery...")
        
        # Common 3GPP TS patterns
        ts_patterns = [
            "TS28104_MdaNrm.yaml",
            "TS28104_MdaReport.yaml",
            "TS28105_AiMlNrm.yaml", 
            "TS28111_FaultNotifications.yaml",
            "TS28111_FaultNrm.yaml",
            "TS28312_IntentExpectations.yaml",
            "TS28312_IntentNrm.yaml",
            "TS28317_RanScNrm.yaml",
            "TS28318_DsoNrm.yaml",
            "TS28319_MsacNrm.yaml",
            "TS28531_NSProvMnS.yaml",
            "TS28531_NSSProvMnS.yaml",
            "TS28532_FileDataReportingMnS.yaml",
            "TS28532_HeartbeatNtf.yaml",
            "TS28532_PerfMnS.yaml",
            "TS28532_ProvMnS.yaml",
            "TS28532_StreamingDataMnS.yaml",
            "TS28536_CoslaNrm.yaml",
            "TS28538_EdgeNrm.yaml",
            "TS28541_5GcNrm.yaml",
            "TS28541_NrNrm.yaml",
            "TS28541_SliceNrm.yaml",
            "TS28550_PerfMeasJobCtrlMnS.yaml",
            "TS28623_ComDefs.yaml",
            "TS28623_FileManagementNrm.yaml",
            "TS28623_GenericNrm.yaml",
            "TS28623_ManagementDataCollectionNrm.yaml",
            "TS28623_MnSRegistryNrm.yaml",
            "TS28623_PmControlNrm.yaml",
            "TS28623_QoEMeasurementCollectionNrm.yaml",
            "TS28623_SubscriptionControlNrm.yaml",
            "TS28623_ThresholdMonitorNrm.yaml",
            "TS28623_TraceControlNrm.yaml",
            "TS29512_Npcf_SMPolicyControl.yaml",
            "TS29514_Npcf_PolicyAuthorization.yaml",
            "TS29520_Nnwdaf_AnalyticsInfo.yaml",
            "TS29520_Nnwdaf_EventsSubscription.yaml",
            "TS29571_CommonData.yaml",
        ]
        
        existing_files = []
        for filename in ts_patterns:
            test_url = f"{self.REPO_BASE_URL}/{branch}/{path}/{filename}"
            try:
                response = requests.head(test_url, timeout=5)
                if response.status_code == 200:
                    existing_files.append(filename)
            except requests.RequestException:
                pass
        
        if existing_files:
            print(f"✓ Pattern discovery found {len(existing_files)} files")
            return existing_files
        
        print("✗ Pattern discovery found no files")
        return []
    
    def download_file(self, filename, branch, path="OpenAPI"):
        """Download a single file from GitLab."""
        url = f"{self.REPO_BASE_URL}/{branch}/{path}/{filename}"
        
        try:
            response = requests.get(url, timeout=30)
            response.raise_for_status()
            return response.content
        except requests.RequestException as e:
            print(f"Error downloading {filename}: {e}")
            return None
    
    def download_release(self, release):
        """Download all OpenAPI files for a specific release."""
        branch = self.get_release_branch(release)
        print(f"Downloading files from 3GPP MnS repository branch: {branch}")
        
        # Get the list of YAML files
        yaml_files = self.get_directory_listing(branch)
        if not yaml_files:
            print(f"No YAML files found for release {release}")
            return False
        
        print(f"Found {len(yaml_files)} YAML files to download")
        
        # Create output directory structure
        release_dir = self.output_dir / f"MnS-{release}-OpenAPI" / "OpenAPI"
        if not self.dry_run:
            release_dir.mkdir(parents=True, exist_ok=True)
        
        downloaded_count = 0
        failed_count = 0
        
        for filename in yaml_files:
            print(f"Processing: {filename}")
            
            if self.dry_run:
                print(f"  [DRY RUN] Would download: {filename}")
                downloaded_count += 1
                continue
            
            # Download the file
            content = self.download_file(filename, branch)
            if content is None:
                failed_count += 1
                continue
            
            # Save the file
            output_file = release_dir / filename
            try:
                with open(output_file, 'wb') as f:
                    f.write(content)
                print(f"  ✓ Downloaded: {output_file}")
                downloaded_count += 1
            except IOError as e:
                print(f"  ✗ Failed to save {output_file}: {e}")
                failed_count += 1
        
        # Summary
        print(f"\nDownload Summary:")
        print(f"  Release: {release}")
        print(f"  Branch: {branch}")
        print(f"  Files downloaded: {downloaded_count}")
        print(f"  Files failed: {failed_count}")
        print(f"  Output directory: {release_dir}")
        
        if downloaded_count > 0:
            print(f"\nTo use these files with the converter:")
            print(f"  openapi-to-rdf convert {release_dir}")
        
        return downloaded_count > 0
    
    def list_available_releases(self):
        """List available releases by checking known branches."""
        # Known releases based on the repository structure
        known_releases = ["Rel-15", "Rel-16", "Rel-17", "Rel-18", "Rel-19"]
        
        print("Available releases:")
        for release in known_releases:
            print(f"  - {release}")
        
        print(f"\nNote: You can also try other release names like 'main' or 'master'")
        return known_releases


def main():
    parser = argparse.ArgumentParser(
        description="Download 3GPP OpenAPI specifications from GitLab repository",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s --release Rel-18 --output-dir assets/
  %(prog)s --release Rel-19 --dry-run
  %(prog)s --list-releases
        """
    )
    
    parser.add_argument(
        "--release",
        help="3GPP release to download (e.g., Rel-18, Rel-19)"
    )
    parser.add_argument(
        "--output-dir",
        default="assets",
        help="Output directory for downloaded files (default: assets/)"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be downloaded without actually downloading"
    )
    parser.add_argument(
        "--list-releases",
        action="store_true",
        help="List available releases and exit"
    )
    
    args = parser.parse_args()
    
    downloader = ThreeGPPDownloader(args.output_dir, args.dry_run)
    
    if args.list_releases:
        downloader.list_available_releases()
        return
    
    if not args.release:
        print("Error: --release is required (use --list-releases to see available options)")
        sys.exit(1)
    
    success = downloader.download_release(args.release)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
