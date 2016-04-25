task :default => :test

def elget
  File.join(ENV['HOME'], '.emacs.d', 'el-get')
end

task :test do
  sh "emacs -Q --no-site-file -batch -L . -L #{elget}/dash -L #{elget}/ert-expectations -l ert-expectations -l org-kanban-tests.el -f batch-expectations"
end
