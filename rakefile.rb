task :default => :test

task :test do
  sh "cask exec ert-runner"
  sh "cask exec ecukes --reporter magnars --quiet"
end
