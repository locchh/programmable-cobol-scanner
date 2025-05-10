# Programmable COBOL Scanner

A flexible, configurable tool for extracting structured information from COBOL programs according to user-defined rules. This tool parses COBOL source code and outputs detailed JSON data about program structure, variables, procedures, and relationships.

## Overview

The Programmable COBOL Scanner uses ANTLR4-based parsing to analyze COBOL programs and extract information based on a YAML configuration file. It's designed to help with legacy code analysis, documentation, migration planning, and understanding complex COBOL codebases.

## Features

- **Configurable Extraction**: Define exactly what elements to extract via YAML configuration
- **Comprehensive Analysis**: Extract information from all COBOL divisions (Identification, Environment, Data, Procedure)
- **Custom Analysis**: Generate insights about variable usage, paragraph calls, and data dependencies
- **Batch Processing**: Process individual files or entire directories (recursively if needed)
- **Structured Output**: Results are provided in well-formatted JSON for easy integration with other tools
- **Statement Formatting**: COBOL statements are formatted for better readability

## Requirements

- Python 3.6+
- ANTLR4 Python runtime
- PyYAML

## Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/programmable-cobol-scanner.git
cd programmable-cobol-scanner

# Install dependencies
pip install antlr4-python3-runtime pyyaml
```

## Usage

```bash
# Process a single COBOL file with default configuration
python cobol_extractor.py path/to/your/program.cob

# Process a single file with custom configuration
python cobol_extractor.py --config your_config.yaml path/to/your/program.cob

# Process all COBOL files in a directory
python cobol_extractor.py path/to/cobol/directory/

# Process all COBOL files in a directory and its subdirectories
python cobol_extractor.py --recursive path/to/cobol/directory/

# Specify an output directory for the JSON files
python cobol_extractor.py --output-dir path/to/output/ path/to/your/program.cob
```

## Configuration

The tool uses a YAML configuration file to determine what information to extract. If no configuration file is provided, a default configuration is used.

Here's an example configuration file structure:

```yaml
output_format: json
extraction:
  identification:
    enabled: true
    elements:
      - program_id
      - author
      - date_written
  data:
    enabled: true
    sections:
      - name: working_storage
        enabled: true
        extract_level_numbers: [1, 5, 10, 15, 20, 77]
        extract_fields: [name, level, picture, value]
      - name: file_section
        enabled: true
        extract_fields: [name, level, organization, access_mode]
  procedure:
    enabled: true
    elements: [paragraphs, sections]
    statement_types: [PERFORM, IF, MOVE, COMPUTE, CALL]
  custom_analysis:
    enabled: true
    elements: [variable_usage, paragraph_calls, data_dependencies]
```

## Output Format

The tool generates JSON output with the following structure:

```json
{
  "metadata": {
    "extraction_date": "2025-05-10T13:28:29",
    "config_file": "yaml-based configuration"
  },
  "identification": {
    "program_id": "SAMPLE",
    "author": "JOHN DOE",
    "date_written": "2023-01-01"
  },
  "data": {
    "working_storage": [
      {
        "level": "01",
        "name": "CUSTOMER-RECORD",
        "picture": "X(100)"
      },
      {
        "level": "05",
        "name": "CUSTOMER-ID",
        "picture": "9(5)"
      }
    ]
  },
  "procedure": {
    "paragraphs": {
      "MAIN-PROCEDURE": {
        "statements": [
          "PERFORM INIT-PROCEDURE",
          "PERFORM PROCESS-DATA UNTIL END-OF-FILE",
          "PERFORM CLEANUP-PROCEDURE",
          "STOP RUN"
        ]
      }
    }
  },
  "custom_analysis": {
    "paragraph_calls": {
      "MAIN-PROCEDURE": ["INIT-PROCEDURE", "PROCESS-DATA", "CLEANUP-PROCEDURE"]
    },
    "data_dependencies": {
      "CUSTOMER-ID": [
        {
          "source": "INPUT-ID",
          "paragraph": "PROCESS-DATA",
          "statement": "MOVE INPUT-ID TO CUSTOMER-ID"
        }
      ]
    }
  }
}
```

## Custom Analysis

The tool can perform several types of custom analysis:

1. **Variable Usage**: Tracks where variables are defined and their properties
2. **Paragraph Calls**: Maps the call hierarchy between paragraphs
3. **Data Dependencies**: Identifies data flow between variables

## Extending the Tool

The tool is designed to be extensible. To add support for additional COBOL elements or analysis types:

1. Extend the `CobolExtractorVisitor` class with new visitor methods
2. Update the configuration structure in `create_default_config()`
3. Add new analysis methods as needed

## License

[MIT License](LICENSE)
