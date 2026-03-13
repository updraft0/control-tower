# Deploying with podman 'quadlets'

This directory contains instructions and template files to deploy controltower using podman [quadlets] (for RedHat 
derivative systems or if you prefer not to use docker).

* Create a user (and user group) that is not `root`
* Create directories `~theuser/db` and `~theuser/ui`
* The configuration file [`application.prod.conf`](./application.prod.conf) needs to be copied and edited with the relevant
  ESI secrets, the encryption secrets and the domain name you're using
  * Then, create the podman secret by doing `cat secret.conf | podman secret create controltower-prod -` (or any other name)
* Generate the SSL certificate (either for CDN reverse proxy or by using something like acme/letsencrypt)
  * Put the private key into secret `controltower-ssl-private-key` and public key into secret ``
* The `backend.container` needs to be copied into `~theuser/.config/containers/systemd` and edited with:
  * The secret name both in the `Secret=<name>` parameter and in the environment variable `BACKEND_CONFIG_SECRET=/run/secrets/<name>`
  * You can also tweak the max RAM usage but at the moment don't go below 3GB or the SDE import is likely to fail
* The `frontend.container` needs to be copied into the same place and edited with:
  * `PublishPort=<your-port>:8080` if you are using a custom port
* Also copy `controltower.network` into the same place (containers)
* Put [`nginx.conf`](./nginx.conf) into `~theuser/ui/nginx.conf`
* Run `systemctl --user daemon-reload`
  * This did not work for me and I needed to run:
    * (once) `sudo loginctl enable-linger $(id -u theuser)`
    * (once) `systemctl —user -M theuser@ enable —now podman.socket`
    * `/usr/lib/systemd/system-generators/podman-system-generator --user ~theuser/.config/systemd/user/`
* You can now start with `systemctl --user enable --now {frontend,backend}.service`

[quadlets]: https://podman-desktop.io/blog/podman-quadlet
