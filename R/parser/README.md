# Field Activity Parser

### Todo

Check the current todo list at - [todo.md](/todo.md)

## Overview

This project is a Shiny application that generates a dynamic user interface based on a JSON schema. It's designed to create forms for various field activities, with support for multiple languages, different input types, and validation support.

## Features

- Dynamic UI generation based on JSON schema
- Multi-language support (English, Finnish, Swedish)
- Support for various input types (select, number, string, textarea)
- Nested object and array handling
- Real-time UI updates on language change

## File Structure

- `fct_parser.R`: Contains functions for parsing the JSON schema
- `fct_ui.R`: Contains functions for creating UI elements
- `app_ui.R`: Defines the main UI structure of the Shiny app
- `app_server.R`: Defines the server-side logic of the Shiny app

## Key Functions

### fct_parser.R

- `flatten_schema()`: Flattens the JSON schema by resolving references
- `parse_json_schema()`: Parses the entire JSON schema
- `parse_event()`: Parses individual events from the schema
- `parse_property()`: Parses individual properties from the schema
- `get_multilingual_field()`: Extracts multilingual values for a given field

### fct_ui.R

- `create_ui()`: Creates UI elements based on the parsed schema
- `create_properties_ui()`: Creates UI elements for properties, handling nested structures
- `create_widget()`: Creates individual UI widgets based on property type
- `get_select_choices()`: Extracts choices for select inputs
- `update_ui_element()`: Updates existing UI elements (e.g., on language change)

### app_ui.R

- `app_ui()`: Defines the main UI structure of the Shiny app
- `golem_add_external_resources()`: Adds external resources to the Shiny app

### app_server.R

- `schema_file_path()`: Gets the path to the schema file
- `app_server()`: Defines the server-side logic of the Shiny app, including event handling and UI updates

## Usage

```R
golem::run_dev()
```
