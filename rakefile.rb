task :default => :test

desc 'prepare development environment'
task :prepare do
  sh "eask install-deps --development"
end

def get_match(content, regexp)
  match = regexp.match(content)
  if !match
    raise 'cannot match'
  end
  return match[1]
end

desc 'test'
task :test, [:verbose] do |t, args|
  melpa_version = get_match(File.read('org-kanban.el', encoding: 'UTF-8'), Regexp.new('Package-Version: (.*)'))
  elisp_version = get_match(File.read('org-kanban.el', encoding: 'UTF-8'), Regexp.new('\\(message "org-kanban (.*)"\\)\\)'))
  eask_version = get_match(File.read('Eask', encoding: 'UTF-8'), Regexp.new('\\(package "org-kanban" "(.*)" ".*"\\)'))
  if melpa_version != elisp_version or
    puts "melpa_version: #{melpa_version}"
    puts "elisp_version: #{elisp_version}"
    puts "eask_version: #{eask_version}"
    raise 'versions inconsistent'
  else
    puts "Testing version #{eask_version}"
  end
  sh 'eask list'
  sh 'eask eval "(org-version t t t)"'
  sh "rm -rf *.elc"
  sh "eask eval \"(byte-compile-file \\\"org-kanban.el\\\")\""
  if args[:verbose] == "true"
    sh "eask exec ecukes --reporter magnars --quiet" # --debug
  else
    sh "eask exec ecukes --reporter dot --quiet "
  end
end

desc 'generate big testfile'
task :generate_big_testfile do
  File.open("big_testfile.org", "w") do |io|
    (1..600).each do |i|
      io.puts("* TODO Testthing #{i}")
    end
  end
end
