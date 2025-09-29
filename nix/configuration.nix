{ config, pkgs, lib, all-cabal-hashes, ... }:

let
  # Use GHC 9.10.1 with updated all-cabal-hashes
  haskellPackages = (pkgs.haskell.packages.ghc9101.override { all-cabal-hashes = all-cabal-hashes; }).extend ((import ./overlay.nix) (pkgs));

  # Build the eventlog-live project
  eventlog-live = haskellPackages.callCabal2nix "eventlog-live" (pkgs.lib.cleanSource ../eventlog-live) { };

  # Build the oddball example
  oddball = haskellPackages.callCabal2nix "oddball" (pkgs.lib.cleanSource ../examples/oddball) { };

  # Build the eventlog-live-otelcol
  eventlog-live-otelcol = haskellPackages.callCabal2nix "eventlog-live-otelcol" (pkgs.lib.cleanSource ../eventlog-live-otelcol) { inherit eventlog-live; };
in
{
  imports = [
   # ./services.nix
  ];

  # Enable virtualization
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 2048; # 2GB RAM
      cores = 2;
      graphics = false;
      forwardPorts = [
        { from = "host"; host.port = 2222; guest.port = 22; }
        { from = "host"; host.port = 3000; guest.port = 3000; }
        { from = "host"; host.port = 3200; guest.port = 3200; }
        { from = "host"; host.port = 9090; guest.port = 9090; }
      ];
    };
  };


  # Basic system configuration
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";

  nixpkgs.config.allowBroken = true;

  # Enable networking
  networking.hostName = "eventlog-live-vm";
  networking.networkmanager.enable = true;

  # Enable SSH for remote access
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";


  # Set root password (for VM access)
  users.users.root.password = "nixos";

  # Enable auto-login as root
  services.getty.autologinUser = "root";

  system.stateVersion = "25.11";

  # Create a dedicated user for eventlog services
  users.users.eventlog = {
    isSystemUser = true;
    group = "eventlog";
    home = "/var/lib/eventlog";
    createHome = true;
  };
  users.groups.eventlog = {};

  # Enable systemd user services
  systemd.services = {
    oddball = {
      description = "Oddball example application";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        User = "eventlog";
        Group = "eventlog";
        WorkingDirectory = "/var/lib/eventlog";

        RuntimeDirectory = "ghc-eventlog-socket";
        RuntimeDirectoryMode = "0755";

        ExecStartPre = "${pkgs.coreutils}/bin/rm -f /run/ghc-eventlog-socket/ghc-eventlog.sock";

        ExecStart = "${oddball}/bin/oddball +RTS -N -l -hT --eventlog-flush-interval=1 -RTS";
        Environment = [
          "GHC_EVENTLOG_SOCKET=/run/ghc-eventlog-socket/ghc-eventlog.sock"
        ];
        Restart = "always";
        RestartSec = 5;
      };
    };

    eventlog-live-otelcol = {
      description = "Eventlog Live OpenTelemetry Collector";
      wantedBy = [ "multi-user.target" ];
      after = [ "oddball.service" ];
      serviceConfig = {
        Type = "simple";
        User = "eventlog";
        Group = "eventlog";
        ExecStart = "${eventlog-live-otelcol}/bin/eventlog-live-otelcol --service-name=oddball --eventlog-socket=/run/ghc-eventlog-socket/ghc-eventlog.sock --otelcol-host=0.0.0.0 --otelcol-port=4317";
        Restart = "always";
        RestartSec = 5;
      };
    };
  };

  # Install required packages
  environment.systemPackages = with pkgs; [
  ];

  # Enable OpenTelemetry Collector service
  services.opentelemetry-collector = {
    enable = true;
    settings = {
      receivers = {
        otlp = {
          protocols = {
            grpc = {
              endpoint = "0.0.0.0:4317";
            };
            http = {
              endpoint = "0.0.0.0:4318";
            };
          };
        };
      };
      processors = {
        batch = {};
      };
      exporters = {
        "otlp/tempo" = {
          endpoint = "0.0.0.0:24317";

          tls = { insecure = true; };
        };
        prometheus = {
          endpoint = "0.0.0.0:9099";
        };
        debug = {
          verbosity = "detailed";
        };
      };
      service = {
        pipelines = {
          traces = {
            receivers = [ "otlp" ];
            exporters = [ "debug" "otlp/tempo" ];
          };
          metrics = {
            receivers = [ "otlp" ];
            processors = [ "batch" ];
            exporters = [ "prometheus" ];
          };
        };
      };
    };
  };

  services.tempo = {
    enable = true;
    configFile = ./config/tempo.yaml;
  };

  # Enable Prometheus for metrics collection
  services.prometheus = {
    enable = true;
    configText = builtins.readFile ./config/prometheus.yaml;
  };

  # --- Dashboards provider (YAML you uploaded) ---
  environment.etc."grafana/provisioning/dashboards/dashboards.yaml".text =
    builtins.readFile ./config/grafana-dashboards.yaml;

  # --- Dashboard JSONs you uploaded ---
  environment.etc."grafana/dashboards/eventlog-heap.json".text =
    builtins.readFile ./config/grafana-dashboards-eventlog-heap.json;
  environment.etc."grafana/dashboards/eventlog-threads.json".text =
    builtins.readFile ./config/grafana-dashboards-eventlog-threads.json;

  # Enable Grafana for visualization
  services.grafana = {
    enable = true;
    provision.enable = true;
    provision.dashboards.settings.providers = [{
        name = "GHC Eventlog Dashboards";
        disableDeletion = true;
        options = {
          path = "/etc/grafana/dashboards";
          foldersFromFilesStructure = true;
        };
      }];
    provision.datasources.path = ./config/grafana-datasources.yaml;
    settings = {
      server = {
        http_port = 3000;
        http_addr = "0.0.0.0";
      };
    };
  };

  # Firewall configuration
#  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    22    # SSH
    3000  # Grafana
    3200  # tempo
    4317  # OTLP gRPC
    4318  # OTLP HTTP
    55679 # zPages
    9099  # Prometheus
    24317 # Tempo
  ];
}
