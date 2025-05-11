FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu
RUN apt-get update && \
    apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
