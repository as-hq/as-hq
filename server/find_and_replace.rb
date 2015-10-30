bah = File.open("client_messages2", "w+")

File.open("client_messages", "r") do |f|
  f.each_line do |line|
    bah.puts line.gsub("INIT_SHEET_ID_ERIC", "INIT_SHEET_ID")
  end
end