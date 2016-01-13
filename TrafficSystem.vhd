LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver 
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    
                                    "1111001" WHEN "00001",    
                                    "0100100" WHEN "00010",    
                                    "0110000" WHEN "00011",    
                                    "0011001" WHEN "00100",    
                                    "0010010" WHEN "00101",   
                                    "0000010" WHEN "00110",    
                                    "1111000" WHEN "00111",    
                                    "0000000" WHEN "01000",    
                                    "0010000" WHEN "01001",    
                                    "0001000" WHEN "01010",    
                                    "0000011" WHEN "01011",   
                                    "0100111" WHEN "01100",   
                                    "0100001" WHEN "01101",   
                                    "0000110" WHEN "01110",    
                                    "0001110" WHEN "01111",    
                                    "1111111" WHEN OTHERS;     

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY TrafficSystem IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0);
      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); 
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); 
      hex0, hex2,hex4,hex6 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END TrafficSystem;

ARCHITECTURE SimpleCircuit OF TrafficSystem IS

   COMPONENT SevenSegment PORT(        
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
   CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL TenHzModCLK:  STD_LOGIC; -- 10 Hz clock
   
   SIGNAL mode,default,SensorNS,SensorEW: std_logic;

   
   SIGNAL mod_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_ten_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE);
   SIGNAL mod_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   SIGNAL mod_ten_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE);
   
   TYPE STATES IS (STATE0, STATE1, STATE2,STATE3);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals od type STATES
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment

   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   --wait counters on 7 seven segment
   SIGNAL NS_counter: UNSIGNED(3 DOWNTO 0);        
   SIGNAL EW_counter: UNSIGNED(3 DOWNTO 0);        
----------------------------------------------------------------------------------------------------

BEGIN
	-- assigning switches to variables
	mode <= sw(17); 
	default <= sw(16);
	SensorNS <=  sw(15);
	SensorEW <= sw(14);
	--assigning lights to variables
 ledr(17 downto 14) <= std_logic_vector(EW_Counter);
 ledg(6 downto 3) <= state_number;
 ledr(8 downto 5) <= std_logic_vector(NS_counter);
 ledr(4 downto 1) <= std_logic_vector(state_counter);
 LEDG(2) <= OneHzBinCLK;
----------------------------------------------------------------------------------------------------
       mod_terminal <= "0000000000000000000000100" ;                  
       mod_Ten_terminal <= "0001001100010010110011111" ;

   
   TenModCLK: PROCESS(clock_50) 
   BEGIN
      IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
         IF (mod_ten_counter = mod_ten_terminal) THEN       -- half period
            TenHzModCLK <= NOT TenHzModCLK;                 -- toggle
            mod_ten_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod_ten_counter <= mod_ten_counter + 1;
         END IF;
      END IF;
   END PROCESS;
	
   ModCLK: PROCESS(tenhzmodclk) 
   BEGIN
      IF (rising_edge(tenhzmodclk)) THEN -- modulus counter increments on rising clock edge
         IF (mod_counter = mod_terminal ) THEN       -- half period
            OneHzModCLK <= NOT OneHzModCLK;                 -- toggle
            mod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod_counter <= mod_counter + 1;
         END IF;
      END IF;
   END PROCESS;

   LEDG(1) <= OneHzModCLK;
   LEDG(0) <= TenHzModCLK;
----------------------------------------------------------------------------------------------------
   FSM: PROCESS(state, OneHzModCLK, TenHzModCLK )
   BEGIN
      CASE state IS
         WHEN STATE0 => --Go State
            state_number <= "0000";
            ledg(8) <= TenHzModCLK;
            ledr(11) <= '0';
            ledr(0) <= '1';
            ledg(7) <= '0';
            
            IF state_counter > "0001" then --solid green for 4 seconds
               ledg(8) <= '1';
               ledr(11) <= '0';
               ledr(0) <= '1';
               ledg(7) <= '0';
               
            END IF;
            IF state_counter = "0101" then --switches state at 6 seconds
               next_state <= STATE1;
            ELSE
               next_state <= STATE0;
            END IF;

         WHEN STATE1 => -Slow/Detect car state
            state_number <= "0001";
               IF state_counter <= "0001" then
				  ledg(8) <= '0';
				  ledr(11) <= tenhzmodclk;
				  ledr(0) <= '1';
				  ledg(7) <= '0';  
               ELSE--returns to green when there is no car
				  ledg(8) <= '1';
				  ledr(11) <= '0';
				  ledr(0) <= '1';
				  ledg(7) <= '0';
               END IF;
            --checks if its day time or car is on the the opposite side when it was the default side to change state
            IF state_counter = "0001" and (mode = '0' or SensorEW = '1' or default = '1')  then 
               next_state <= STATE2;
            ELSE
               next_state <= STATE1;
            END IF;   
        WHEN STATE2 => --Opposite lane reverse state 1 and 0
            state_number <= "0010";
            --flashing green 2 sec
            ledg(8) <= '0';
            ledr(11) <= '1';
            ledr(0) <= '0';
            ledg(7) <= TenHzModCLK;
            
            IF state_counter > "0001" then--solid green 4 sec
               ledg(8) <= '0';
               ledr(11) <= '1';
               ledr(0) <= '0';
               ledg(7) <= '1';
               
            END IF;
            IF state_counter = "0101" then--state change 6 sec
               next_state <= STATE3;
            ELSE
               next_state <= STATE2;
            END IF;

         WHEN OTHERS =>
            state_number <= "0011";
               IF (state_counter <= "0001")then
               ledg(8) <= '0';
               ledr(11) <= '1';
               ledr(0) <= tenhzmodclk;
               ledg(7) <= '0';
               
               ELSE--returns to green when there is no car
				  ledg(8) <= '0';
				  ledr(11) <= '1';
				  ledr(0) <= '0';
				  ledg(7) <= '1';
               END IF;
            --checks if its day time or car is on the the opposite side when it was the default side to change state
            IF state_counter = "0001" and (mode = '0' or SensorNS = '1' or default = '0')  then
               next_state <= STATE0;
            ELSE
               next_state <= STATE3;
            END IF;     
      END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(OneHzModCLK, state) -- creates sequential logic to latch the state
   BEGIN
      IF (rising_edge(OneHzModCLK)) THEN
         
                              -- on the rising edge of clock the current state is updated with next state
         state_counter <= state_counter + 1;    -- on the rising edge of clock the current counter is incremented if state is STATE1
		IF (state /= next_state) THEN --resets counter on state change
			state_counter<="0000";
			state <= next_state; 
        END IF;
        IF (state_counter = "0111" ) THEN --resets counter on state change
			state_counter<="0000";
        END IF;
        --wait counter sensors
        IF SensorEW = '1' and (state_number = "0000" or state_number = "0001") then
			EW_counter <= EW_counter + 1;
		ELSE
		    EW_counter <= "0000";
        END IF;
        IF SensorNS = '1' and (state_number = "0010" or state_number = "0011") then
			NS_counter <= NS_counter + 1;
		ELSE
		    NS_counter <= "0000";
        END IF;
     END IF;
     --resets counters
     IF SensorEW = '1' and (state_number = "0010" or state_number = "0011") then
		    EW_counter <= "0000";
        END IF;
     IF SensorNS = '1' and (state_number = "0000" or state_number = "0001") then
		    NS_counter <= "0000";
        END IF;
        
   END PROCESS;
----------------------------------------------------------------------------------------------------
   --displays the counters on 7 segments
   D7S0: SevenSegment PORT MAP( state_number, '0', hex0 );
   D7S4: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );
   D7S6: SevenSegment PORT MAP( std_logic_vector(NS_counter), '0', hex4 );
   D7S8: SevenSegment PORT MAP( std_logic_vector(EW_counter), '0', hex6 );
END SimpleCircuit;
