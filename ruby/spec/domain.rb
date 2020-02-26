require_relative '../src/class_persistent_methods'
require_relative '../src/persistent'

class Pikachu
  include Persistent
  has_one Integer, named: :level, from: 10 , to: 100, validate: proc{level * 2 > 25}, default: 15 #, not_equal_to: 20
  has_one String, named: :evolution, no_blank: true
  has_one Boolean, named: :wild
  attr_accessor :type # Este atributo NO es persistible

  def self.build_ash_pikachu
    pikachu = Pikachu.new
    pikachu.level = 25
    pikachu.evolution = "Raichu"
    pikachu.wild = false
    pikachu.type = "Electric"
    pikachu
  end

  def self.build_wild_pikachu
    pikachu = Pikachu.new
    pikachu.level = 15
    pikachu.evolution = "Raichu"
    pikachu.wild = true
    pikachu.type = "Electric"
    pikachu
  end
end

class Bulbasaur
  include Persistent
  has_one Integer, named: :level
  has_one String, named: :level
end

class Squartle
  include Persistent
  has_one Integer, named: :level
end

class Evolution
  include Persistent
  has_one String, named: :value
end

class Charmander
  include Persistent
  has_one Evolution, named: :evolution
end

class Attack
  include Persistent
  has_one String, named: :name
end

class Pidgeotto
  include Persistent
  has_many Attack, named: :attacks
end

class Psyduck
  include Persistent
  has_one Integer, named: :level
  has_one Evolution, named: :evolution
  has_many Attack, named: :attacks
  attr_accessor :type

  def self.build_a_complete_psyduck
    psyduck = Psyduck.new
    psyduck.level = 20
    evo = Evolution.new
    evo.value = "Golduck"
    psyduck.evolution = evo
    coletazo = Attack.new
    coletazo.name = "Coletazo"
    hipnosis = Attack.new
    hipnosis.name = "Hipnosis"
    psyduck.attacks = [coletazo, hipnosis]
    psyduck.type = "Agua y Psiquico"
    psyduck
  end
end

class Pokemon
  include Persistent
  has_one Integer, named: :level, from: 10 , to: 100, validate: proc{level * 2 > 25}, not_equal_to: 20
  has_one String, named: :evolution, no_blank: true
  has_one Boolean, named: :wild
  attr_accessor :type # Este atributo NO es persistible

  def self.builder(level, evolution, type, wild)
    p = Pokemon.new
    p.level = level
    p.evolution = evolution
    p.type = type
    p.wild = wild
    return p
  end

  def ==(other_pokemon)
    id == other_pokemon.id &&
        level == other_pokemon.level &&
        evolution == other_pokemon.evolution &&
        wild == other_pokemon.wild
    type == other_pokemon.type
  end
end

class Abra
  include Persistent
  has_one String, named: :evolution, no_blank: false # No se chequea si evolution es ""
  has_one Integer, named: :level

  def self.build(evolution, level)
    abra = Abra.new
    abra.evolution = evolution
    abra.level = level
    abra
  end
end

class Snorlax < Pokemon
  include Persistent
  has_one Integer, named: :sleep_time
end

module Psychic
  include Persistent
  has_one Integer, named: :concentration_level
end

class Mewtwo
  include Persistent
  include Psychic
  has_one Boolean, named: :wild
end

class Pichu
  include Persistent
  def initialize(level, evolution)
    @level = level
    @evolution = evolution
  end
end

