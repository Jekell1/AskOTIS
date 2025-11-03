	   78 OTCS-ERROR                VALUE 0.
	   78 OTCS-OK                   VALUE 1.
	   78 OTCS-START                VALUE 2.
	   78 OTCS-TERMINATE            VALUE 3.
	   78 OTCS-BWSCAN-PAGE          VALUE 100.
	   78 OTCS-STMAIN-DATA          VALUE 101.
	   78 OTCS-PING-DATA            VALUE 102.



	   78 OTCR-ERROR                VALUE 0.
	   78 OTCR-OK                   VALUE 1.
	   78 OTCR-DONE                 VALUE 2.
	   78 OTCR-TERMINATE            VALUE 3.
	   78 OTCR-PING                 VALUE 4.
	   78 OTCR-BWSCAN-START         VALUE 100.			
	   78 OTCR-BWSCAN-REPLY         VALUE 101.
	   78 OTCR-STMAIN               VALUE 110.			
	   
	   78 WO-BFR-MAX                VALUE 16384.
	   
	   
       01 WINOTIS-ERROR             PIC 9.


       01 WOS-MSG-ID                PIC 9(4).
       01 WOS-BUFFER-LEN            PIC 9(4).
       01 WOS-BUFFER.           
           03 WOS-BUFFER-VALUE          
		                  PIC X(WO-BFR-MAX).
           03 WOS-BUFFER-ARRAY REDEFINES 
		                  WOS-BUFFER-VALUE
               OCCURS WO-BFR-MAX TIMES  
			                        PIC X.        
			   

       01 WOR-MSG-ID                PIC 9(4).
       01 WOR-BUFFER-LEN            PIC 9(4).
       01 WOR-BUFFER.           
           03 WOR-BUFFER-VALUE         
		                  PIC X(WO-BFR-MAX).
           03 WOR-BUFFER-ARRAY REDEFINES 
		                  WOR-BUFFER-VALUE
               OCCURS WO-BFR-MAX TIMES  
			                        PIC X.        

       01  WO-ERROR-MESSAGE.
           03  WO-ERR-PROG-ID       PIC X(10).
           03  WO-ERR-DESC          PIC X(80).
