      * COPYMEMBER: LIBLP/FDMAIN.CMD
       01  LEGEND-01                     PIC X(39) VALUE
           "F1v,F2^,F3-u1,F4-u5,F5-d5,F6-d1,F7-DONE".
       01  LEGEND-02                     PIC X(39) VALUE
           "F3-u1, F4-u5, F5-d5, F6-d1, F7-DONE".
       01  TEST-PROFILE                  PIC X.
           88  VALID-PROFILE             VALUE " "
                                         "a" thru "z"
                                         "A" thru "Z".
