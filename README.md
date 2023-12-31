This repo contains data and code for the
collaboration between UConn, TextProject,
Emelex, and FSU on machine-based methods
of teaching and learning concerning properties
of curricular texts.

Team Members:
Devin Kearns, UConn
Nathan Crock, Emelex
Elfrieda Hiebert, TextProject
Mohammed Razzakuddin, UConn
Madhuri Malyala, FSU
Mario Shontz, FSU
Pramodh Sairam, FSU
Giulio Martini, FSU
Matt Cooper Borkenhagen, FSU

## Getting Started with the Development Environment
Welcome to the ConnTextUL Data Repository project. This guide will help you set up your development environment using our new Dev Container and Poetry for package management. Follow these steps to get started.

### Prerequisites
- Install [Docker Desktop](https://www.docker.com/products/docker-desktop/) on your machine.
- Install [Visual Studio Code](https://code.visualstudio.com/download).
- Install the [Remote - Containers extension](https://code.visualstudio.com/docs/devcontainers/containers) in VS Code.

### Setting Up the Development Container
1. Clone the Repository: First, clone this repository to your local machine.

```bash
git clone https://github.com/MCooperBorkenhagen/ConnTextUL_data.git
cd ConnTestUL_data
```

2. Open in VS Code: Open the cloned repository in Visual Studio Code.
3. Reopen in Container: VS Code may prompt you to reopen the project in a container. If so, click on "Reopen in Container". Otherwise, press F1, type "Remote-Containers: Reopen in Container", and press Enter.
4. Wait for the Container to Build: This may take a few minutes as it sets up the environment for the first time.

### Working with Poetry
1. Accessing the Terminal: Once the Dev Container is running, you can access the terminal in VS Code. This terminal is already inside the container.
2. Using Poetry: To manage Python packages, use Poetry commands in the terminal. For example, to add a new package:

```bash
poetry add [package-name]
```

3. Verify Changes: Inspect the pyproject.toml file to ensure that the appropriate package has been added.
4. Add Dev Packages: To add a library specifically for development purposes, include the **--dev** flag
```bash
poetry add [package-name] --dev
```
### Additional Features
- Jupyter Notebooks: Our Dev Container comes with Jupyter extensions. You can create and run notebooks directly in VS Code.
- R Support: For R projects, the container is pre-configured with necessary R packages.
- Linting and Formatting: Use the provided linters and formatters like Black for Python and tools for R to maintain code quality.