#!/usr/bin/env bash

set -e

echo "Building Eventlog Live NixOS VM..."

# Check if we're in the right directory
if [ ! -f "configuration.nix" ]; then
    echo "Error: configuration.nix not found. Please run this script from the nix directory."
    exit 1
fi

# Build the VM
echo "Building VM configuration..."

nix build .#vm
./result/bin/run-eventlog-live-vm-vm

echo "VM built successfully!"
echo ""
echo "To run the VM:"
echo "  ./result/bin/run-nixos-vm"
echo ""
echo "Once running, you can access:"
echo "  - Grafana: http://localhost:3000 (admin/admin)"
echo "  - Prometheus: http://localhost:9090"
echo "  - SSH: ssh root@localhost -p 2222 (password: nixos)"
echo ""
echo "To stop the VM, press Ctrl+A then X in the QEMU console."
