task :default => :test

task :test do
  sh "cask install"
  sh "cask exec ecukes --reporter magnars --quiet"
end
