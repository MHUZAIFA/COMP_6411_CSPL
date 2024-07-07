# Python Database Server

## Table of Contents
- [Introduction](#introduction)
- [Features](#features)
- [Design](#design)
- [Screenshots](#screenshots)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Running the Code](#build)
- [Contributors](#contributors)
- [Contributing](#contributing)
- [License](#license)
- [Acknowledgements](#acknowledgements)

## Introduction:
  Welcome to the project. The project involves creating a simple database server and client application in Python. 

## Features:
  - Server loads customer records from `data.txt` on startup.
  - Implements a client/server architecture using Python's `socket` and `socketserver` modules.
  - Supports operations like finding, adding, deleting, and updating customer records.
  - Provides error checking for input validation based on specified criteria.

## Design:
    The server loads data from a text file into memory and provides a simple client interface for managing customer records. The database server will handle all data operations, ensuring data integrity and consistency.

## Getting Started:
  ### Prerequisites:
    Python 3.x installed on your system.

  ### Installation
    git clone https://github.com/MHUZAIFA/COMP_6411_CSPL
    cd A1```

## Running the Code:
  - Start the Server:
    ```bash
    python3 server.py &
    ```
    Use the `&` to run the server as a background process.

  - Start the Client:
    - Run `client.py` from the command line to connect to the running server.

  - Stopping the Server: 
      - Use the `kill` command to stop the server process:
        ```bash
        kill <server_process_id>
        ```

## Contributors:
  - [Mohammed Huzaifa](https://github.com/MHUZAIFA)

## Contributing:

We welcome contributions! If you would like to contribute to this project, please read our contribution guidelines.

## License:
  This project is licensed under the MIT License. See the LICENSE file for details.

## Acknowledgements:
  - A big thanks to the Python documentation and community for providing valuable resources.
  - Special thanks to the course instructors and TAs for their guidance and support.
