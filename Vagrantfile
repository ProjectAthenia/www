########################################################################################################################
## Vagrant Configuration File
########################################################################################################################

########################################################################################################################
## Bootstrapping / Requirements
########################################################################################################################
Vagrant.require_version ">= 1.9.0"

unless Vagrant.has_plugin?("vagrant-hostsupdater")
  raise 'vagrant-hostsupdater is not installed! Run "vagrant plugin install vagrant-hostsupdater"'
end

Vagrant.configure("2") do |config|

    ####################################################################################################################
    ## Shared Ansible Configuration Method
    ####################################################################################################################
    def shared_ansible_config(ansible)
        ansible.playbook = "ansible/playbook.yml"
        ansible.host_key_checking = true
        ansible.become = true
    end
    
    ####################################################################################################################
    ## admin.lingwave.com ** PRODUCTION **
    ####################################################################################################################

    config.vm.define "prod" do |prod|

        unless Vagrant.has_plugin?("vagrant-digitalocean")
          raise 'vagrant-digitalocean is not installed! Run "vagrant plugin install vagrant-digitalocean"'
        end

        ## Local git-ignored credential file: ./vagrant-credentials.rb
        ## Should contain (without dashes) (where the provider token is the private Digital Ocean Token)
        ## --------
        ## PROVIDER_TOKEN = 'xyz'
        ## --------
        load 'vagrant-credentials.rb'

        prod.vm.hostname = "admin.lingwave.com"

        prod.vm.box = "digital_ocean"
        prod.vm.box_url = "https://github.com/devopsgroup-io/vagrant-digitalocean/raw/master/box/digital_ocean.box"
  	    prod.vm.synced_folder ".", "/vagrant", disabled: "true"

        prod.vm.provider :digital_ocean do |provider, override|
            override.ssh.private_key_path = "/Users/brycemeyer/.ssh/id_rsa"
            override.ssh.username = "vagrant"
            provider.ssh_key_name = "Bryce"
            provider.token = PROVIDER_TOKEN
            provider.image = "ubuntu-16-04-x64"
            provider.region = "nyc3"
            provider.size = "s-1vcpu-1gb"
            provider.backups_enabled = "true"
        end

        ## if we're upping this, do the following tasks:
        if ARGV[0] == 'up'
            ## add the key
            prod.vm.provision "file", source: "/Users/brycemeyer/.ssh/id_rsa.pub", destination: "/tmp/id_rsa.pub"
            ## add the user
            prod.vm.provision "shell", path: "vagrant-do-provision.sh"
        end

        prod.vm.provision "ansible" do |ansible|
            shared_ansible_config ansible
            ansible.host_key_checking = false ## override for local
            ansible.extra_vars = {
                server_name: prod.vm.hostname,
                server_env: "production",
                notification_email: "dev@lingwave.com"
            }
        end
    end
end