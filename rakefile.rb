task :default => :test

desc 'prepare'
task :prepare do
  sh "cask install"
end

desc 'test'
task :test do
  sh "cask exec ecukes --reporter magnars --quiet"
end
