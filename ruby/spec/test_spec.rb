describe "Tests TADP Metaprogramacion ORM" do

  describe 'has_one' do
    it 'genera accessors para los atributos persistibles' do
      pikachu = Pikachu.new
      pikachu.level = 25
      pikachu.evolution = "Raichu"
      expect(pikachu.level).to eq 25
      expect(pikachu.evolution).to eq "Raichu"
    end

    it 'pisa un atributo persistible ya definido en la misma clase' do
      bulbasaur = Bulbasaur.new
      bulbasaur.level = 20
      # Me aprovecho de la validación de tipos para testear esta funcionalidad (no encuentro otra opción)
      # La segunda declaración de level pisa a la primera, así que su tipo es String y no Integer,
      # por lo que lanza un error de validación.
      expect{bulbasaur.validate!}.to raise_exception(TypeValidationError)
      bulbasaur.level = "20"
      expect{bulbasaur.validate!}.not_to raise_exception(TypeValidationError)
    end

    it 'pisa un atributo persistible ya definido por herencia' do


    end
  end

  describe "Una instancia de clase persistible" do
    let(:pikachu) {Pikachu.new}

    it 'entiende los mensajes save!, refresh! y delete!' do
      expect([:save!, :refresh!, :forget!].all? {|msg| pikachu.respond_to? msg}).to eq true
    end
  end

  describe 'Una clase persistible (no instancia)' do
    it 'no incluye atributos no persistibles' do
      expect(Pikachu.attr_persistibles_symbols).not_to include(:type)
    end

    it 'me sabe decir los nombres (simbolos) de sus atributos persistibles' do
      # match_array no se preocupa por el orden de los elementos en el array
      expect(Pikachu.attr_persistibles_symbols).to match_array([:id, :level, :evolution, :wild])
    end

    it 'NO entiende los mensajes de instancia save!, refresh! y forget!' do
      expect{Pikachu.save!}.to raise_exception(NoMethodError)
      expect{Pikachu.refresh!}.to raise_exception(NoMethodError)
      expect{Pikachu.forget!}.to raise_exception(NoMethodError)
    end

    it "tiene asociada una tabla con su mismo nombre" do
      expect(Pikachu.table.name).to eq Pikachu.name
    end
  end

  describe "save!" do
    before(:each) do
      Squartle.table.clear
      Charmander.table.clear
      Evolution.table.clear
      Pidgeotto.table.clear
      Attack.table.clear
      Snorlax.table.clear
      Mewtwo.table.clear
    end

    it 'le asigna un id a la instancia salvada' do
      squartle = Squartle.new
      squartle.level = 25 # Para que no rompa
      expect(squartle.id).to be_nil
      squartle.save!
      expect(squartle.id).to_not be_nil
    end

    it 'persiste instancias de una clase persistible con atributos primitivos' do
      # mi_clase = Class.new do
      #   def asdf
      #
      #   end
      # end
      #
      # mi_clase.new
      expect(Squartle.all_instances.size).to eq 0 # No hay registros en la tabla Squartle
      squartle = Squartle.new
      squartle.level = 25
      squartle.save!
      expect(Squartle.all_instances.size).to eq 1 # Hay un registro en la tabla Squartle
      otro_squartle = Squartle.new
      otro_squartle.level = 10
      otro_squartle.save!
      expect(Squartle.all_instances.size).to eq 2 # Hay dos registros en la tabla Squartle
    end

    it 'persiste instancias de una clase persistible con atributos complejos cascadeando el salvado' do
      charmander = Charmander.new
      evo = Evolution.new
      evo.value = "Charmeleon"
      charmander.evolution = evo
      charmander.save! # Guarda a charmander pero también a evo
      expect(Charmander.all_instances.size).to eq 1
      expect(Evolution.all_instances.size).to eq 1
      # Recupero la instancia de charmander persistida (y con ella también la instancia evolución)
      charmander_saved = Charmander.find_by_id(charmander.id).first
      expect(charmander_saved.evolution.value).to eq "Charmeleon"
    end

    it 'persiste instancias de una clase persistible con atributos compuestos (many) cascadeando el salvado' do
      pidgeotto = Pidgeotto.new
      tornado = Attack.new
      tornado.name = "Tornado"
      picotazo = Attack.new
      picotazo.name = "Picotazo"
      pidgeotto.attacks = [tornado, picotazo]
      pidgeotto.save! # Guarda a charmander pero también a evo
      expect(Pidgeotto.all_instances.size).to eq 1
      expect(Attack.all_instances.size).to eq 2
      pidgeotto_saved = Pidgeotto.find_by_id(pidgeotto.id).first
      tornado_saved = pidgeotto_saved.attacks.first
      expect(tornado_saved.name).to eq tornado.name
    end

    it 'persiste instancias de una clase persistible con atributos heredados' do
      snorlax = Snorlax.new
      snorlax.level = 40
      snorlax.evolution = "None"
      snorlax.wild = true
      snorlax.type = "Normal"
      snorlax.sleep_time = 120
      snorlax.save!
      expect(Snorlax.all_instances.size).to eq 1
      snorlax_saved = Snorlax.find_by_id(snorlax.id).first
      expect(snorlax_saved.level).to eq snorlax.level
    end

    it 'persiste instancias de una clase persistible con atributos incluidos (por mixin)' do
      mewtwo = Mewtwo.new
      mewtwo.wild = false
      mewtwo.concentration_level = 1050
      mewtwo.save!
      expect(Mewtwo.all_instances.size).to eq 1
      mewtwo_saved = Mewtwo.find_by_id(mewtwo.id).first
      expect(mewtwo_saved.concentration_level).to eq mewtwo.concentration_level
    end

  end

  describe 'refresh!' do
    before(:each) do
      Psyduck.table.clear
      Evolution.table.clear
      Attack.table.clear
    end

    let(:psyduck){
      Psyduck.build_a_complete_psyduck
    }

    it 'recarga a memoria la ultima version en disco (persistida) de un objeto' do
      psyduck.save!
      psyduck.level = 35
      psyduck.evolution.value = "SuperPsyduck"
      psyduck.attacks.first.name = "Pistola de agua"
      psyduck.refresh!
      expect(psyduck.level).to eq 20
      expect(psyduck.evolution.value).to eq "Golduck"
      expect(psyduck.attacks.map {|att| att.name}).to match_array(["Coletazo", "Hipnosis"])
      # Al hacer el refresh, no debemos "tocar" los atributos no persistibles en memoria (como type)
      expect(psyduck.type).to eq "Agua y Psiquico"
    end

    it 'falla si el objeto nunca fue persistido' do
      expect{psyduck.refresh!}.to raise_exception(ObjectNotPersistedError)
    end

    describe 'forget!' do
      it 'elimina a un objeto persistido de disco' do
        psyduck.save!
        expect(psyduck.id).to_not be_nil
        expect(Psyduck.all_instances.size).to eq 1
        psyduck.forget!
        expect(psyduck.id).to be_nil
        expect(Psyduck.all_instances.size).to eq 0
        expect(Evolution.all_instances.size).to eq 0
        expect(Attack.all_instances.size).to eq 0
        # También se eliminan todas las entradas correspondientes de la tabla intermedia
        # Pysduck_Attacks pero no sé si tengo forma de acceder a ella desde acá para hacer
        # el assert (se puede chequear a mano en la carpeta db
        expect(psyduck.evolution.value).to eq "Golduck" # El objeto en memoria sigue "vivo"
      end
    end
  end

  describe 'all_instances' do
    before(:each) do
      Pokemon.table.clear
      Pikachu.table.clear
    end

    it 'retorna todas las instancias persistidas de una clase' do
      pikachu = Pokemon.builder(25, "Raichu", "Eléctrico", false)
      charmander = Pokemon.builder(15, "Charmeleon", "Fuego", true)
      squartle = Pokemon.builder(10, "Wartortle", "Agua", true)
      pikachu.save!
      charmander.save!
      # No guardamos a squartle, así que sólo hay dos registros en la tabla Pokemon
      expect(Pokemon.all_instances.size).to eq 2
    end

    it 'retorna objetos funcionales (instancias reales de la clase persistida)' do
      pikachu = Pokemon.builder(25, "Raichu", "Eléctrico", false)
      pikachu.save!
      pikachu_saved = Pokemon.find_by_id(pikachu.id).first
      # Puedo enviarle mensajes a pikachu_saved, el objeto recuperado de disco
      pikachu_saved.level = 35
      pikachu_saved.evolution = "SuperRaichu"
      expect(pikachu_saved.level).to eq 35
      expect(pikachu_saved.evolution).to eq "SuperRaichu"
    end
  end

  describe 'find_by' do
    before(:each) do
      Pikachu.table.clear
    end

    it 'recupera las instancias de acuerdo al mensaje enviado' do
      ash_pikachu = Pikachu.build_ash_pikachu
      ash_pikachu.save!
      wild_pikachu = Pikachu.build_wild_pikachu
      wild_pikachu.save!
      expect(Pikachu.all_instances.size).to eq 2
      expect(Pikachu.find_by_wild(true).size).to eq 1
      wild_pikachu_saved = Pikachu.find_by_wild(true).first
      expect(wild_pikachu_saved.wild).to eq true
    end

    it 'by_evolution' do
      pikachu = Pokemon.builder(25, "Raichu", "Electrico", false)
      charmander = Pokemon.builder(15, "Charmeleon", "Fuego", true)
      pikachu.save!
      charmander.save!
      pikachu_saved = Pokemon.find_by_evolution("Raichu").first
      charmander_saved = Pokemon.find_by_evolution("Charmeleon").first
      # Seteamos a mano el atributo type porque no es persistible (no se guarda)
      pikachu_saved.type = "Electrico"
      charmander_saved.type = "Fuego"
      expect(pikachu).to eq pikachu_saved
      expect(charmander).to eq charmander_saved
    end
  end

  describe "Validaciones" do
    let(:pikachu){
      Pikachu.build_ash_pikachu
    }

    it 'se pasan todas las validaciones de todos los atributos persistibles' do
      expect(pikachu.validate!).to eq true
    end

    it 'se lanza la excepcion TypeValidationError al validar un objeto con un atributo
      cuyo tipo declarado no coincide con el tipo seteado' do
      # El atributo level esta declarado de tipo Integer y le seteamos un String
      pikachu.level = "Un nivel"
        expect{pikachu.validate!}.to raise_exception(TypeValidationError)
    end

    it 'se lanza la excepcion ToValidationError cuando un atributo con la
      validacion to tiene seteado un valor superior al permitido' do
      # has_one Integer, named: :level, from: 10 , to: 100, ...
      pikachu.level = 200 # 200 > 100 -> lanza la excepción
      expect{pikachu.validate!}.to raise_exception(ToValidationError)
      pikachu.level = 50 # 50 < 100 -> Permitido
      expect(pikachu.validate!).to eq true
    end

    it 'se lanza la excepcion FromValidationError cuando un atributo con la
      validacion from tiene seteado un valor inferior al permitido' do
      # has_one Integer, named: :level, from: 10 , to: 100, ...
      pikachu.level = 5 # 5 < 10 -> lanza la excepción
      expect{pikachu.validate!}.to raise_exception(FromValidationError)
      pikachu.level = 15 # 15 > 10 -> Permitido
      expect(pikachu.validate!).to eq true
    end

    it 'se lanza la excepcion NoBlankValidationError cuando un atributo con la
      validacion no_blank en true contiene una cadena vacia' do
      # has_one String, named: :evolution, no_blank: true
      pikachu.evolution = ""
      expect{pikachu.validate!}.to raise_exception(NoBlankValidationError)
      pikachu.evolution = "Raichu"
      expect{pikachu.validate!}.to_not raise_exception(NoBlankValidationError)
    end

    it 'se omite la validacion no_blank en un atributo cuando se le pasa el flag en false' do
      abra = Abra.build("", 25)
      expect{abra.validate!}.to_not raise_exception(NoBlankValidationError)
    end

    it 'se lanza la excepcion ValidateBlockError cuando un atributo con la
      validacion validate no satisface el bloque pasado por parametro' do
      # has_one Integer, ..., validate: proc{level * 2 > 25}
      pikachu.level = 11 # 22 < 25 -> No cumple el bloque
      expect{pikachu.validate!}.to raise_exception(ValidateBlockError)
      pikachu.level = 25 # 50 > 25 -> Pasa el bloque
      expect(pikachu.validate!).to eq true
    end

    it 'se carga un atributo con su valor default al llamar a validate!' do
      pikachu.level = nil
      pikachu.validate!
      expect(pikachu.level).to eq 15
    end
  end
end