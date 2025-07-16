# ./Rakefile (root)

# Order of subdirectories to build (build libraries first)
SUBDIRS = ['src', 'tests']

# Default task: build all subdirectories
task :default => :all

desc "Build all subdirectories"
task :all do
  puts "--- Building all targets ---"
  SUBDIRS.each do |dir|
    puts "\n==> Building in #{dir}"
    cd dir do
      sh 'rake'
    end
  end
  puts "\n--- Build finished ---"
end

desc "Run all tests"
task :test do
  # Make sure everything is built before running tests
  Rake::Task["all"].invoke

  puts "\n--- Running all tests ---"
  cd 'tests' do
    sh 'rake run'
  end
end

desc "Clean all subdirectories"
task :clean do
  puts "--- Cleaning all targets ---"
  # Clean up temporary files that might be created first
  # sh 'rm -f libexample.so fcntl.lock flock.lock'

  SUBDIRS.each do |dir|
    puts "\n==> Cleaning in #{dir}"
    cd dir do
      sh 'rake clean'
    end
  end
  puts "\n--- Clean finished ---"
end
