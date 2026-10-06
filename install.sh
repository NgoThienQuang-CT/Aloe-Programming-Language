#!/usr/bin/env bash
set -euo pipefail

# 1. Auto-load OPAM environment if dune is not yet in PATH
if ! command -v dune &>/dev/null; then
  if command -v opam &>/dev/null; then
    eval "$(opam env 2>/dev/null)" || true
  fi
fi

# 2. Fallback: check ~/.opam if running under sudo or in a minimal shell
if ! command -v dune &>/dev/null; then
  USER_HOME="${HOME}"
  if [[ -n "${SUDO_USER:-}" ]]; then
    USER_HOME=$(getent passwd "$SUDO_USER" | cut -d: -f6)
  fi
  for opam_bin in "$USER_HOME"/.opam/*/bin; do
    if [[ -x "$opam_bin/dune" ]]; then
      export PATH="$opam_bin:$PATH"
      break
    fi
  done
fi

if [[ $EUID -eq 0 ]]; then
  DEFAULT_PREFIX="/usr/local"
else
  DEFAULT_PREFIX="$HOME/.local"
fi

PREFIX="${PREFIX:-$DEFAULT_PREFIX}"
BINDIR="$PREFIX/bin"
ACTION="install"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --prefix)
      PREFIX="$2"
      BINDIR="$PREFIX/bin"
      shift 2
      ;;
    --system)
      PREFIX="/usr/local"
      BINDIR="/usr/local/bin"
      shift
      ;;
    --uninstall)
      ACTION="uninstall"
      shift
      ;;
    -h|--help)
      echo "Usage: ./install.sh [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --prefix <path>   Set custom installation prefix (default: ~/.local)"
      echo "  --system          Install globally to /usr/local/bin (may require sudo)"
      echo "  --uninstall       Remove aloe binary from installation prefix"
      echo "  -h, --help        Show this help message"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

if [[ "$ACTION" == "uninstall" ]]; then
  if [[ -f "$BINDIR/aloe" ]]; then
    echo "[-] Removing $BINDIR/aloe..."
    rm -f "$BINDIR/aloe"
    echo "✓ Aloe successfully uninstalled."
  else
    echo "Aloe is not installed in $BINDIR."
  fi
  exit 0
fi

if ! command -v dune &>/dev/null; then
  echo "Error: 'dune' could not be found."
  echo "Make sure OPAM is installed and run: eval \$(opam env)"
  exit 1
fi

echo "==> Building Aloe in release mode..."
dune build --profile release bin/main.exe

echo "==> Installing binary to $BINDIR/aloe..."
# Only use sudo if directory requires root permissions
SUDO=""
if [[ ! -w "$PREFIX" && $EUID -ne 0 ]]; then
  SUDO="sudo"
fi

$SUDO mkdir -p "$BINDIR"
$SUDO install -m 755 _build/default/bin/main.exe "$BINDIR/aloe"

echo "✓ Successfully installed Aloe to $BINDIR/aloe!"

# PATH check
if [[ ":$PATH:" != *":$BINDIR:"* ]]; then
  echo ""
  echo "Notice: $BINDIR is not in your PATH."
  echo "Add the following line to your ~/.bashrc or ~/.zshrc:"
  echo ""
  echo "    export PATH=\"$BINDIR:\$PATH\""
  echo ""
fi