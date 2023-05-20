# Dark theme for VS Code
Dark color theme for C and Zig.

# How to install:
code --install-extension dark-1.0.0.vsix

# How to create a package:
sudo pacman -S npm
sudo npm install -g @vscode/vsce
cd MyExtension
vsce package

#VS Code syntax inspection
CTRL + SHIFT + P -> "Developer: Inspect Editor Tokens and Scopes"
