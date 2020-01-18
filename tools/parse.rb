require 'time'
require 'reverse_markdown'


DELIM_POST = /^--------$/
DELIM_CONTENT = /^-----$/

class Post
  def initialize(metadata, body)
    @date = Time.strptime(metadata[:date], "%m/%d/%Y %H:%M:%S")
    @status = metadata[:status].downcase.to_sym
    @body = body
    @metadata = metadata
  end
  def basename
    @date.strftime("%Y-%m-%d_%H-%M-%S.md")
  end
  def write(dir)
    cats = @metadata[:category].map { |c| c.downcase }.join(", ")
    md_body = ReverseMarkdown.convert @body
    File.open(File.join(dir, basename), "w") do |f|
      f.puts("---")
      f.puts("title: #{@metadata[:title]}")
      f.puts("date: #{@date.strftime('%Y-%m-%d')}")
      f.puts("tags: #{cats}")
      f.puts("---")
      f.puts("")
      f.write(md_body)
    end
  end
end

def parse_metadata(input)
  input.strip!.each_line.inject({category: []}) do |m, line|
    key, value = line.split(":", 2)
    key_sym = key.downcase.to_sym
    value_str = value.strip!
    if key_sym == :category
      m[:category].push(value_str)
    else
      m[key_sym] = value_str
    end
    m
  end
end

def main
  content = File.read(ARGV[0])
  output = ARGV[1]
  content.split(DELIM_POST).map do |post|
    meta, body = post.split(DELIM_CONTENT)
    metadata = parse_metadata(meta)
    body_trim = body[6..body.size]
    p = Post.new(metadata, body_trim)
    p.write(output)
  end
end

main
