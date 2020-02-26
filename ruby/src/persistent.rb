require_relative 'instance_persistent_methods'
require_relative 'class_persistent_methods'
require_relative 'persistables_attributes/types/PrimitiveAttribute'

module Persistent
  # Como hace un include Persistent al usarlo los metodos que se agreguen son de clase
  def self.included(base)
    base.include InstancePersistentMethods # (Crud) incluye los metodos del module como metodos de instancia
    base.extend ClassPersistentMethods
  end
end
