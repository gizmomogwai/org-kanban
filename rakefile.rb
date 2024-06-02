task :default => :test

desc 'prepare'
task :prepare do
  sh "cask install"
end

def get_match(content, regexp)
  match = regexp.match(content)
  if !match
    raise 'cannot match'
  end
  return match[1]
end

desc 'test'
task :test do
  melpa_version = get_match(File.read('org-kanban.el', encoding: 'UTF-8'), Regexp.new('Package-Version: (.*)'))
  elisp_version = get_match(File.read('org-kanban.el', encoding: 'UTF-8'), Regexp.new('\\(message "org-kanban (.*)"\\)\\)'))
  cask_version = get_match(File.read('Cask', encoding: 'UTF-8'), Regexp.new('\\(package "org-kanban" "(.*)" "Kanban for org-mode."\\)'))
  if melpa_version != elisp_version or
    melpa_version != cask_version or
    elisp_version != cask_version
    puts "melpa_version: #{melpa_version}"
    puts "elisp_version: #{elisp_version}"
    puts "cask_version: #{cask_version}"
    raise 'versions inconsistent'
  else
    puts "Testing version #{cask_version}"
  end
  sh 'cask list'
  sh 'cask eval "(org-version t t t)"'
  sh "rm -rf *.elc"
  sh "cask eval \"(byte-compile-file \\\"org-kanban.el\\\")\""
  sh "cask exec ecukes --quiet --reporter progress"
end

desc 'generate big testfile'
task :generate_big_testfile do
  File.open("big_testfile.org", "w") do |io|
    (1..600).each do |i|
      io.puts("* TODO Testthing #{i}")
    end
  end
end
