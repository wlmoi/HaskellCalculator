# HaskellCalculator
> A GTK-Based Calculator in Haskell

## Prerequisites
Ensure Haskell is installed on your system. You can download it from [Haskell.org](https://www.haskell.org/downloads/) or manage it via your system's package manager.

## GTK Development Libraries
The program requires GTK development libraries to build and run the graphical interface:

- **Linux**: Install GTK development packages using your package manager (`libgtk-3-dev` on Debian/Ubuntu, `gtk3-devel` on Fedora).
- **Windows**: GTK installation may require MSYS2 or WSL setup for development. Refer to GTK installation guides specific to Windows.
- **macOS**: GTK installation is possible through Homebrew or MacPorts. Refer to GTK installation guides specific to macOS.

## Steps to Run

### Clone or Download the Repository

Clone this repository to your local machine using Git:

```bash
git clone https://github.com/wlmoi/HaskellCalculator

Alternatively, download the repository as a ZIP archive and extract it to a local folder.

Navigate to the Project Directory
Open a terminal or command prompt.

cd path/to/your/repository

Compile the Haskell Program
Compile the HaskellCalculator.hs file using GHC (Glasgow Haskell Compiler):

ghc --make HaskellCalculator.hs

This command generates an executable file named Calculator (or Calculator.exe on Windows) in the same directory.

Run the Calculator
Execute the compiled executable from the terminal or command prompt:

./HaskellCalculator   # On Linux or macOS
HaskellCalculator.exe # On Windows

The calculator window should now appear on your screen.

Using the Calculator
Enter numbers and perform operations (+, -, *, /) using the calculator buttons.
Use ‘=’ to calculate the result of the entered expression.
‘C’ clears the entire entry.
‘CE’ clears the last entered character.
‘<-’ performs a backspace operation.
Exiting the Program
Close the calculator window to exit the program.
Alternatively, press q in the terminal/command prompt where the program is running to terminate it.

Example
After following the above steps, the GTK window titled “Calculator” will open. Use the calculator interface to perform calculations and observe the results. Follow on-screen prompts and error messages for correct usage.

Notes
Ensure all dependencies, including Haskell, GTK libraries, and any other required packages, are correctly installed before compiling and running the program.
Adjust installation and setup steps based on your specific operating system and development environment.
Please replace `path/to/your/repository` with your actual project directory path. If you need any further assistance or adjustments, feel free to ask!
