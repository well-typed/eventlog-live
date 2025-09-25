# Eventlog Live NixOS VM Configuration

This directory contains a NixOS configuration that builds a VM with:

- The `oddball` example application running as a NixOS service
- The OpenTelemetry Collector running as a NixOS service
- Prometheus for metrics collection
- Grafana for visualization

## Prerequisites

- Nix with flakes enabled
- QEMU (for running the VM)

## Building and Running the VM

### Option 1: Using the flake (recommended)

```bash
# Build the VM
nix build .#vm

# Run the VM
./result/bin/run-nixos-vm
```

### Option 2: Using nixos-rebuild

```bash
# Build the VM
nixos-rebuild build-vm -I nixos-config=./configuration.nix

# Run the VM
./result/bin/run-nixos-vm
```

## Accessing the Services

Once the VM is running, you can access:

- **Grafana**: http://localhost:3000 (admin/admin)
- **Prometheus**: http://localhost:9090
- **OpenTelemetry Collector zPages**: http://localhost:55679

## SSH Access

You can SSH into the VM:

```bash
ssh root@localhost -p 2222
# Password: nixos
```

## Services

The following services are automatically started:

1. **oddball**: The example Haskell application that generates random numbers
2. **eventlog-live-otelcol**: Collects GHC eventlog data and exports to OpenTelemetry
3. **opentelemetry-collector**: Receives and processes telemetry data
4. **prometheus**: Collects and stores metrics
5. **grafana**: Provides visualization dashboards

## Configuration

The configuration is split into two files:

- `configuration.nix`: Main NixOS configuration
- `services.nix`: Custom systemd services

## Troubleshooting

If services fail to start, check the logs:

```bash
# SSH into the VM and check service status
systemctl status oddball
systemctl status eventlog-live-otelcol
systemctl status opentelemetry-collector

# Check logs
journalctl -u oddball -f
journalctl -u eventlog-live-otelcol -f
```

## Customization

To use a different example application, modify the `configuration.nix` file:

1. Change the `oddball` package reference to point to your desired example
2. Update the service configuration accordingly
3. Adjust environment variables as needed

## Ports

The following ports are exposed:

- 22: SSH
- 3000: Grafana
- 4317: OTLP gRPC
- 4318: OTLP HTTP
- 55679: zPages
- 9090: Prometheus
