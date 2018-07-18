
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
  org_kanban_melpa_version = get_match(File.read('org-kanban.el', encoding: 'UTF-8'), Regexp.new('Package-Version: (.*)'))
  org_kanban_elisp_version = get_match(File.read('org-kanban.el', encoding: 'UTF-8'), Regexp.new('\\(message "org-kanban (.*)"\\)\\)'))
  cask_version = get_match(File.read('Cask', encoding: 'UTF-8'), Regexp.new('\\(package "org-kanban" "(.*)" "Kanban for org-mode."\\)'))
  if org_kanban_melpa_version != org_kanban_elisp_version or org_kanban_elisp_version != cask_version
    puts "org_kanban_melpa_version: #{org_kanban_melpa_version}"
    puts "org_kanban_elisp_version: #{org_kanban_elisp_version}"
    puts "org_kanban_cask_version: #{cask_version}"
    raise 'versions inconsistent'
  else
    puts "Testing version #{cask_version}"
  end
  sh 'cask list'
  sh 'cask eval "(org-version t t t)"'
  sh "cask exec ecukes --verbose --debug --reporter magnars"
end
