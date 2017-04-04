(* CSCI243 Modeling Project *)
(*A game to be played between two or more people. Each character has a weapon, armor, and class associated with it. To play aggame, each person creates a player for themselves and that is stored into a tuple that holds the name of the player and his/her stats. The function FightDuel initiates a battle between two players. The players can choose one of three moves for that battle: Attack, Block, and Heal. Attack decreases the other player's health by an amount specified by his/her attack stat. Block decreases the amount the other player's attack affects him/her and also increases his/her attack stat. Heal increases his/her health. The player that reach 0 health loses and the player last standing wins. However, if both/all players reach 0 health after one round, then everyone loses.

    EXAMPLE:
    val asher = makePlayer("Asher the Great",Greatsword, Leather, Assassin);
    val tim = makePlayer("Tim the Humble",Shield, Iron, Monk);
    FightDuel(asher, Attack, tim, Block);
*)

(*The gear that the players choose to determine the stats of their characters.*)
datatype weapon = Greatsword | Axe | Spear | Shield;
datatype armor = Iron | Chainmail | Leather | Loincloth;
datatype class = Assassin | Berserker | Knight | Monk;

(*A type defined by the gear that the player is wearing.*)
datatype character = Character of weapon * armor * class;

(*The available actions that a player can make during a battle.*)
datatype action = Attack | Block | Heal;

(*returns the attack associated with a certain character configuration.*)
fun getAttack(c) =
  let fun wAttack(Character(Greatsword, y, z)) = 9.0
     | wAttack(Character(Axe, y, z)) = 8.0
     | wAttack(Character(Spear, y, z)) = 7.0
     | wAttack(Character(Shield, y, z)) = 6.0;
   fun aAttack(Character(x, Loincloth, z)) = 9.0
     | aAttack(Character(x, Leather, z)) = 8.0
     | aAttack(Character(x, Chainmail, z)) = 7.0
     | aAttack(Character(x, Iron, z)) = 6.0;
   fun cAttack(Character(x, y, Assassin)) = 9.0
     | cAttack(Character(x, y, Berserker)) = 8.0
     | cAttack(Character(x, y, Knight)) = 7.0
     | cAttack(Character(x, y, Monk)) = 6.0;
  in
    wAttack(c) + aAttack(c) + cAttack(c)
  end;

(*returns the defense associated with a certain character configuration.*)
fun getDefense(c) =
  let fun wDefense(Character(Greatsword, y, z)) = 1.0
	   | wDefense(Character(Axe, y, z)) = 2.0
	   | wDefense(Character(Spear, y, z)) = 3.0
	   | wDefense(Character(Shield, y, z)) = 4.0;
	 fun aDefense(Character(x, Loincloth, z)) = 1.0
	   | aDefense(Character(x, Leather, z)) = 2.0
	   | aDefense(Character(x, Chainmail, z)) = 3.0
	   | aDefense(Character(x, Iron, z)) = 4.0;
	 fun cDefense(Character(x, y, Assassin)) = 1.0
	   | cDefense(Character(x, y, Berserker)) = 2.0
	   | cDefense(Character(x, y, Knight)) = 3.0
	   | cDefense(Character(x, y, Monk)) = 4.0;
  in
    wDefense(c) + aDefense(c) + cDefense(c)
  end;

(*Creates a tuple which will be used as the player.*)
fun makePlayer(n:string, w:weapon, a:armor, c:class) = 
	(n, ref (getDefense(Character(w,a,c))), ref (getAttack(Character(w,a,c))),ref 100.0);

(*Returns the amount of damage that will be done given certain attack and defense stats of the players. The attacker cannot do less than 0 damage.*)
fun damage(attack,defense,ratio) = 
    if (attack-(defense/ratio)) < 0.0 then 0.0 else attack-(defense/ratio);

(*Checks the current state of the game. Prints an update (players’ health), or announces a winner if one/both of the players has reached 0.0 health.*)
fun whoWins(p1:string*real ref* real ref*real ref, p2:string*real ref*real ref*real ref) = 
    if !(#4p1) <= 0.0 andalso !(#4p2) <= 0.0 then 
    print("Both players lose.\n") else
    if !(#4p1) <= 0.0 then
    print(#1p2 ^ " wins!\n") else
    if !(#4p2) <= 0.0 then
    print(#1p1 ^ " wins!\n") else
    print("The game goes on.\n" ^ #1p1 ^ "'s health: " ^ Real.toString(!(#4p1)) ^ "\n" ^ #1p2 ^ "'s health: " ^ Real.toString(!(#4p2)) ^ "\n");

(*One round of the game. Each player gives what action they want to perform. Stats and health are updated as needed.*)
fun FightDuel(player1, Attack, player2, Attack) =
      ((#4player1) := !(#4player1) - damage(!(#3player2), !(#2player1), 4.0);
        (#4player2) := !(#4player2) - damage(!(#3player1), !(#2player2), 4.0);
        whoWins(player1,player2))
  | FightDuel(player1, Attack, player2, Block) =
       ((#4player2) := !(#4player2) - damage(!(#3player1), !(#2player1), 2.0);
       (#3player2) := !(#3player2) + 5.0;
        whoWins(player1,player2))
  | FightDuel(player1, Attack, player2, Heal) =
       ((#4player2) := !(#4player2) - damage(!(#3player1), !(#2player2), 4.0) + 25.0;
        if !(#4player2) > 100.0 then (#4player2) := 100.0 else ())
  | FightDuel(player1, Block, player2, Attack) =
       ((#4player1) := !(#4player1) - damage(!(#3player2), !(#2player1), 2.0);
       (#3player1) := !(#3player1) + 5.0;
        whoWins(player1,player2))
  | FightDuel(player1, Block, player2, Block) = 
       ((#3player1) := !(#3player1) + 5.0;
        (#3player2) := !(#3player2) + 5.0;
        whoWins(player1,player2))
  | FightDuel(player1, Block, player2, Heal) =
       ((#3player1) := !(#3player1) + 5.0;
        (#4player2) := !(#4player2) + 25.0;
        if !(#4player2) > 100.0 then (#4player2) := 100.0 else ();
        whoWins(player1,player2))
  | FightDuel(player1, Heal, player2, Attack) =
       ((#4player1) := !(#4player1) - damage(!(#3player2), !(#2player1), 4.0) + 25.0;
        if !(#4player1) > 100.0 then (#4player1) := 100.0 else ();
        whoWins(player1,player2))
  | FightDuel(player1, Heal, player2, Block) =
       ((#4player1) := !(#4player1) + 25.0;
        if !(#4player1) > 100.0 then (#4player1) := 100.0 else ();
        (#3player2) := !(#3player2) + 5.0;
        whoWins(player1,player2))
  | FightDuel(player1, Heal, player2, Heal) =
       ((#4player1) := !(#4player1) + 25.0;
        if !(#4player1) > 100.0 then (#4player1) := 100.0 else ();
        (#4player2) := !(#4player2) + 25.0;
        if !(#4player2) > 100.0 then (#4player2) := 100.0 else ();
        whoWins(player1,player2));
