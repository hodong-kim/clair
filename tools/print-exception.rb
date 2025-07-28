#!/usr/bin/env ruby
# frozen_string_literal: true

errno_names = %w(
EBADF
ECONNRESET
EFAULT
EIO
EINTEGRITY
EBUSY
EINVAL
EAGAIN
EISDIR
EOPNOTSUPP
EOVERFLOW
)

TOOLS_DIR = __dir__
FILENAME = "#{TOOLS_DIR}/../src/clair-error.ads"

unless File.exist?(FILENAME)
  warn "Error: Source file not found at '#{FILENAME}'"
  exit 1
end

errno_map = {}
EXCEPTION_REGEX = /^\s*([A-Za-z_]+)\s*:\s*exception;\s*--\s*([A-Z0-9_]+)/

File.foreach(FILENAME) do |line|
  if (match_data = line.match(EXCEPTION_REGEX))
    ada_exception_name = match_data[1]
    c_constant_name    = match_data[2]
    errno_map[c_constant_name] = ada_exception_name
  end
end

if errno_names.include? "EWOULDBLOCK"
  i = errno_names.index("EWOULDBLOCK")
  errno_names[i] = "EAGAIN"
end

errno_names.uniq!

puts "case errno_code is"

errno_names.each do |errno_name|
  exception = errno_map[errno_name]

  if exception.nil? or exception.empty?
    raise "#{errno_name} not found"
  end

  puts "  when errno_h.#{errno_name} =>"
  puts "    raise Clair.Error.#{exception} with error_msg;"
end

puts "  when others =>"
puts "    raise Clair.Error.Unmapped_Error with error_msg;"
puts "end case;"
