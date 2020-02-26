require_relative 'help/require_module_persistible'
require_relative 'persistent'

# Metodos de clase
module ClassPersistentMethods
  attr_reader :table

  def self.extended(base)
    base._create_table_class
    base._init_persistables_attr
  end

  def has_one(type, hash_info_attr)
    _create_accessors(hash_info_attr[:named])
    if _is_primitive?(type)
      primitive_attr = PrimitiveAttribute.new(type, hash_info_attr)
      attr_persistibles.merge!({primitive_attr.named => primitive_attr})
    else
      complex_attr = ComplexAttribute.new(type, hash_info_attr)
      attr_persistibles.merge!({complex_attr.named => complex_attr})
    end
  end

  def has_many(type, hash_info_attr)
    _create_accessors(hash_info_attr[:named])
    compound_attr = CompoundAttribute.new(type, hash_info_attr, self)
    attr_persistibles.merge!({compound_attr.named => compound_attr})
  end

  def all_instances
    instances = table.all_entries
    descendants.each do |descendant|
      instances += descendant.all_instances
    end
    instances
  end

  def attr_persistibles
    @attr_persistibles
  end

  def all_attr_persistibles
    attr_persistibles_all = attr_persistibles
    # Si la superclase hace extend ModulePersistible entonces ya responde a :attr_persistibles,
    # pero puede que no tenga ningun atributo persistible (marcado con has_one), en cuyo caso
    # retornará nil, así que debemos preguntar primero. Nunca puede retornar [].
    ancestors_list = ancestors.drop(1) # Descarto la singleton class
    ancestors_list.each do |ancestor|
      if (ancestor.include? Persistent) && !ancestor.attr_persistibles.nil? # Si es nil, entonces no tiene atributos persistibles
        # Invertimos el orden de precedencia (se pisan los atributos más lejanos a la clase actual) -> reverse_merge
        attr_persistibles_all.merge!(ancestor.attr_persistibles) { |key, my_attr, ancestor_attr| my_attr }
      end
    end
    attr_persistibles_all
  end

  def all_attr_persistables_simples
    all_attr_persistibles.select do |key, attr|
      (attr.is_a? PrimitiveAttribute) || (attr.is_a? ComplexAttribute)
    end
  end

  def all_attr_persistables_compounds
    all_attr_persistibles.select {|key, attr| attr.is_a? CompoundAttribute}
  end

  # find_by_<message>(condition)
  def method_missing(name, *args, &block) #OK
    if name.to_s.start_with? "find_by"
      sub_message = name.to_s.gsub("find_by_", "") # Obtenemos el nombre del mensaje que acompaña a find_by_mensaje
      condition = args[0]
      all_instances.select {|an_instance| an_instance.send(sub_message.to_sym) == condition}
    else
      super
    end
  end

  # Al hacer un override de method_missing debemos hacer un override de respond_to_missing? para
  # mantener la consistencia del mensaje respond_to?
  def respond_to_missing?(method, include_private = false) #OK
    (method.to_s.start_with? "find_by_") || super
  end

  def attr_persistibles_symbols(all=false)
    all ? all_attr_persistibles.map {|key, attr| key} : attr_persistibles.map {|key, attr| key}
  end

  def exists_id?(id)
    not id.nil?
  end

  #private
  def _is_primitive?(type)
    type == String || type.ancestors.include?(Numeric) || type.ancestors.include?(Boolean)
  end

  def _create_accessors(attr_named)
    attr_accessor attr_named
  end

  def _create_table_class
    @table = Table.new(self) # Instancio mi clase Table (es un wrapper de Table de TADB)
  end

  def _init_persistables_attr
    @attr_persistibles = {} # Atributo de la clase persistible (no de las instancias)
    #Agregamos @id como atributo persistible
    @attr_persistibles[:id] = (PrimitiveAttribute.new(String, {named: :id}))
  end

  def descendants
    ObjectSpace.each_object(Class).select { |klass| klass < self && !klass.to_s.start_with?("#") }
  end
end