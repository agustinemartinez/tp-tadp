# Abro Symbol para agregarle el siguiente método auxiliar
class Symbol
  # Convierte un symbol en un atributo. ej: :level.to_attr => @level
  def to_attr
    "@" + self.to_s
  end
end
