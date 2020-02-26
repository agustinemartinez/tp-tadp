class String
  # Convierte un atributo string en un symbol. ej: ":@var" -> :var
  def attr_to_sym
    self.gsub("@", "")
  end
end