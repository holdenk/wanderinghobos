# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|
  mount_point = "/home/vagrant/src"
  config.vm.box = "icfp2012"
  config.vm.box_url = "https://github.com/downloads/mgregson/icfp2012-vagrant/icfp2012.box"
  config.vm.share_folder "v-root", mount_point, "."
  config.vm.provision :shell do |shell|
    shell.inline = "apt-get install -y `cat $1/PACKAGES-TESTING`"
    shell.args = mount_point
  end
end
