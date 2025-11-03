      * COPYMEMBER: LIBLP/DSINQ.CMD
       01  DAILY-PROMPTS-I-AM-WORLD      PIC x(40)  VALUE
           " LOSS                       INCREASE    ".

       01  DAILY-PROMPTS.
           03  FILLER                    PIC x(40)  VALUE
           "    ---- OUTSTANDING -----   MTD  GAIN /".
           03  DAILY-PROMPTS-I-AM        PIC x(40)  VALUE
           " LOSS                      NFR:  60/90  ".

           03  FILLER                    PIC x(40)  VALUE
           "     Balance       No   Avg    Balance  ".
           03  FILLER                    PIC x(40)  VALUE
           "     No                                 ".

           03  FILLER                    PIC x(40)  VALUE
           "    AVERAGE     --------------- LOANS MA".
           03  FILLER                    PIC x(40)  VALUE
           "DE MTD --------------    NET   AVERAGES ".

           03  FILLER                    PIC x(40)  VALUE
           "   Lnamt Trm    $Volume    Nb   %Os     ".
           03  FILLER                    PIC x(40)  VALUE
           "Fb   %Os     Pb   %Os    Nb    Fb    Pb ".

           03  FILLER                    PIC x(40)   VALUE 
           "       UNITS --- % ------- RECENCY --- %".
           03  FILLER                    PIC x(40)   VALUE 
           "    UNITS --- % --- CONTRACTUAL --- % ".
       01  DAILY-PROMPT-NO               REDEFINES DAILY-PROMPTS.
           03  DAILY-PROMPTS-NO          OCCURS 5 TIMES
                                         PIC x(80).

