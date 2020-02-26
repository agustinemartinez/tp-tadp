require 'tadb'
require_relative '../open_classes/OpenSymbol'
require_relative 'IntermediateTable'

class Table
  attr_reader :real_table, :table_class

  def initialize(a_class)
    @real_table = TADB::DB.table(a_class.name)
    @table_class = a_class
  end

  # Inserta una instancia completa, incluyendo sus atributos primitivos y sus atributos
  # complejos, cascadeando la inserción.
  def insert_instance(an_instance)
    # Me armo el hash con los atributos persistibles SIMPLES (has_one) y después lo inserto
    attr_persistibles_hash = {}
    an_instance.class.all_attr_persistables_simples.each do |key, attr|
        attr.save_attr!(an_instance, attr_persistibles_hash)
      end
    id = real_table.insert(attr_persistibles_hash) # Retorna el id asignado al insertar
    an_instance.instance_variable_set(:@id, id) # Seteamos el id obtenido a la instancia
    id # Retornamos el id (se utiliza en insert)
  end

  # Inserta un atributo de tipo colección cascadeando
  def insert_collection(an_instance)
    an_instance.class.all_attr_persistables_compounds.each do |key, attr|
      sub_instances = an_instance.instance_variable_get(attr.named.to_attr)
      # Paso sub_instances.first para poder obtener el tipo de la colección
      intermediate_ids_var_name = attr.set_intermediate_ids_var(an_instance, sub_instances.first)
      sub_instances.each do |sub_instance|
        id_sub_instance = sub_instance.save! # Llamada recursiva
        sub_instance.instance_variable_set(:@id, id_sub_instance) # Seteamos el id obtenido a la sub_instancia
        id_intermediate = attr.intermediate_table.insert(an_instance, sub_instance)
        an_instance.instance_variable_set(
            intermediate_ids_var_name,
            an_instance.instance_variable_get(intermediate_ids_var_name).push(id_intermediate))
      end
    end
  end

  def insert(an_instance)
    id_instancia = insert_instance(an_instance)
    # Recién después de insertar la instancia (charmander) y obtener su id, puedo insertar en la tabla intermedia
    insert_collection(an_instance)
    id_instancia # Retornamos el id de la instancia insertada originalmente
  end

  def read(id)
    table_class.find_by_id(id).first
  end

  def all_entries
    # Asumiendo que la clase persistible tiene constructor sin parámetros
    real_table.entries.map do |entry| # entry es un hash atributoPersistible-valor
      an_instance = table_class.new # Creo una nueva instancia del objeto
      an_instance.class.all_attr_persistibles.each do |key, attr|
        attr.load_attr(an_instance, entry)
      end
      an_instance
    end
  end

  def delete!(an_instance)
    an_instance.class.all_attr_persistibles.each {|key, attr| attr.delete!(an_instance)}
    real_table.delete(an_instance.instance_variable_get(:@id))
  end

  def clear
    real_table.clear if real_table.entries.size > 0
  end

  def name
    table_class.name
  end

end
