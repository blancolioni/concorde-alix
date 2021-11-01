package Concorde.Identifiers is

   subtype Object_Identifier is String (1 .. 12);

   function Next_Identifier return Object_Identifier;

end Concorde.Identifiers;
