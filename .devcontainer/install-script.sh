curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

echo "source /home/vscode/.ghcup/env" >> ~/.bashrc

npm install -g @google/gemini-cli

sudo apt-get update && \
  sudo apt-get install -y apt-transport-https ca-certificates gnupg curl && \
  curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg && \
  echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
  sudo apt-get update && sudo apt-get install -y google-cloud-cli
