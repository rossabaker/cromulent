module "nixos_image" {
  source = "github.com/tweag/terraform-nixos/google_image_nixos"
  nixos_version = "20.03"
}

provider "google" {
  project = "rossabaker-348417"
  region  = "us-central1"
  zone    = "us-central1-c"
}

resource "google_project_service" "compute" {
  project = "rossabaker-348417"
  service = "compute.googleapis.com"
}

resource "google_project_service" "oslogin" {
  project = "rossabaker-348417"
  service = "oslogin.googleapis.com"
}

resource "google_compute_instance" "vm_instance" {
  name         = "abe"
  machine_type = "e2-micro"

  boot_disk {
    initialize_params {
      image = module.nixos_image.self_link
    }
  }

  metadata = {
    enable-oslogin = "TRUE"
    nix-configuration = file("configuration.nix")
  }

  network_interface {
    # A default network is created for all GCP projects
    network = "default"
    access_config {
    }
  }

  tags = ["http-server"]
}

resource "google_compute_firewall" "http-server" {
  name    = "http-server"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["80", "443"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["http-server"]
}

resource "google_project_iam_binding" "os-login-admin-users" {
  project = "rossabaker-348417"
  role = "roles/compute.osAdminLogin"

  members = [
    "user:rossabaker@gmail.com"
  ]
}
