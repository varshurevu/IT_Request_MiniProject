 
/*------------------------------------------------------------------------
   File        : ItRequest
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Varshitha.R
   Created     : Fri May 06 12:33:29 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS businesslogic.ItRequest: 
    {ui/itrequest.i}.

    DEFINE PUBLIC PROPERTY cEmail AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY cFirstName AS CHARACTER NO-UNDO 
        GET.
        SET.
         

    DEFINE PUBLIC PROPERTY clastName AS CHARACTER NO-UNDO 
        GET.
        SET. 
    
    
    DEFINE PUBLIC PROPERTY csatus AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY cTicketSatus AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY cPhone AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY iusrTicketId AS INTEGER NO-UNDO 
        GET.
        SET. 
    

    DEFINE PUBLIC PROPERTY usrNum AS INTEGER NO-UNDO 
        GET.
        SET. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC ItRequest (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /*    METHOD PUBLIC VOID AddTicket(INPUT TABLE ttuserTicket,input usrid as integer):                                            */
    /*        DEFINE VARIABLE iTicketid AS INTEGER NO-UNDO.                                                                         */
    /*        FIND FIRST ttuserTicket WHERE ttuserTicket.tusrid =usrid  NO-LOCK NO-ERROR. //check the temp table                    */
    /*        IF AVAILABLE ttuserTicket THEN                                                                                        */
    /*        DO:                                                                                                                   */
    /*                                                                                                                              */
    /*            Add_UsrTicket:                                                                                                    */
    /*            DO TRANSACTION                                                                                                    */
    /*                ON ERROR UNDO Add_UsrTicket , LEAVE Add_UsrTicket:                                                            */
    /*                iusrTicketId = NEXT-VALUE (nestUsrTicketId) .                                                                 */
    /*                FIND FIRST userTicket WHERE UserTicket.UserTicketID = THIS-OBJECT:iusrTicketId EXCLUSIVE-LOCK NO-ERROR.       */
    /*                IF NOT AVAILABLE userTicket THEN                                                                              */
    /*                DO:                                                                                                           */
    /*                    CREATE UserTicket.                                                                                        */
    /*                                                                                                                              */
    /*                    ASSIGN                                                                                                    */
    /*                        UserTicket.usrid        = ttuserTicket.tusrid                                                         */
    /*                        /*                        UserTicket.ticketid     = ttuserTicket.tticketId*/                          */
    /*                        UserTicket.UserTicketID = iusrTicketId                                                                */
    /*                        UserTicket.startdate    = TODAY                                                                       */
    /*                        UserTicket.ticketstatus = ttuserTicket.tticketStatus.                                                 */
    /*                    MESSAGE UserTicket.UserTicketID                                                                           */
    /*                        VIEW-AS ALERT-BOX.                                                                                    */
    /*                    FIND FIRST Ticket WHERE Ticket.category = ttuserTicket.tcategory EXCLUSIVE-LOCK NO-ERROR.                 */
    /*                    IF  AVAILABLE Ticket THEN                                                                                 */
    /*                    DO:                                                                                                       */
    /*                        ASSIGN                                                                                                */
    /*                            /*                               iTicketid = Ticket.ticketid*/                                    */
    /*                            UserTicket.ticketid      = Ticket.ticketid                                                        */
    /*                            Ticket.ticketdiscription = ttuserTicket.tTicketDiscription.                                       */
    /*                    END.                                                                                                      */
    /*                    find first TicketAssignee WHERE TicketAssignee.category = ttuserTicket.tcategory EXCLUSIVE-LOCK  NO-ERROR.*/
    /*                    if AVAILABLE TicketAssignee THEN                                                                          */
    /*                    DO:                                                                                                       */
    /*                        UserTicket.assignee = TicketAssignee.code.                                                            */
    /*                    END.                                                                                                      */
    /*                                                                                                                              */
    /*                END.                                                                                                          */
    /*                ELSE                                                                                                          */
    /*                DO:                                                                                                           */
    /*                    UNDO,THROW NEW Progress.Lang.AppError("not updated orderline",1).                                         */
    /*                END.                                                                                                          */
    /*            END.                                                                                                              */
    /*        end.                                                                                                                  */
    /*        RETURN.                                                                                                               */
    /*                                                                                                                              */
    /*        CATCH e AS Progress.Lang.Error:                                                                                       */
    /*            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).                                                         */
    /*          //  nestUsrTicketId = nestUsrTicketId - 1.                                                                          */
    /*        END CATCH.                                                                                                            */
    /*                                                                                                                              */
    /*        FINALLY:                                                                                                              */
    /*                                                                                                                              */
    /*        END FINALLY.                                                                                                          */
    /*                                                                                                                              */
    /*    END METHOD.                                                                                                               */

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID AddTicketusr(INPUT TABLE ttuserTicket):
        DEFINE VARIABLE iticketid AS INTEGER NO-UNDO.
        FIND FIRST ttuserTicket WHERE ttuserTicket.tusrTicketId = THIS-OBJECT:iusrTicketId NO-LOCK NO-ERROR. //check the temp table 
        IF AVAILABLE ttuserTicket THEN 
        DO:
                        
            Add_UsrTicket:
            DO TRANSACTION
                ON ERROR UNDO Add_UsrTicket , LEAVE Add_UsrTicket:
                   
                FIND FIRST UserTicket WHERE UserTicket.UserTicketID = THIS-OBJECT:iusrTicketId EXCLUSIVE-LOCK NO-ERROR. 
                
                IF NOT AVAILABLE UserTicket THEN
                DO:
                                                             
                   
                    CREATE UserTicket.  
                      
                    ASSIGN
                        UserTicket.usrid        = ttuserTicket.tusrid
                        /*                                                UserTicket.ticketid     = ttuserTicket.tticketId*/
                        UserTicket.UserTicketID = ttuserTicket.tusrTicketId
                        UserTicket.startdate    = TODAY
                        UserTicket.ticketstatus = ttuserTicket.tticketStatus.
                   
                    FIND FIRST Ticket WHERE Ticket.category = ttuserTicket.tcategory   USE-INDEX indx_ticketcat EXCLUSIVE-LOCK NO-ERROR.
                    IF  AVAILABLE Ticket THEN
                    DO:
                        ASSIGN
                            UserTicket.ticketid = Ticket.ticketid.
                    /*                            iticketnum =Ticket.ticketid*/
                                              
                    /*                            Ticket.ticketdescription = ttuserTicket.tTicketDiscription.*/
                    END.
                    find first TicketAssignee WHERE TicketAssignee.category = ttuserTicket.tcategory USE-INDEX idx_ticketAsscat EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE TicketAssignee THEN
                    DO:
                        UserTicket.assignee = TicketAssignee.code.
                        
                    END.
                END.

                /*                END.*/
                ELSE 
                DO:
                    UNDO,THROW NEW Progress.Lang.AppError("not added userticket",1).
                END.
            END. 
        END.       
        RETURN.

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
          //  nestUsrTicketId = nestUsrTicketId - 1.
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID AddUser(INPUT-OUTPUT TABLE ttUser):
        FIND FIRST ttUser WHERE ttUser.ttusrid =THIS-OBJECT:usrNum  NO-LOCK NO-ERROR. //check the temp table 
        IF AVAILABLE ttUser THEN 
        DO:
            /*            MESSAGE "cls entry"   */
            /*                VIEW-AS ALERT-BOX.*/
            Add_Ticket:
            DO TRANSACTION
                ON ERROR UNDO Add_Ticket , LEAVE Add_Ticket:
                MESSAGE THIS-OBJECT:usrNum
                    VIEW-AS ALERT-BOX.
                   
                FIND FIRST Users WHERE Users.usrid = THIS-OBJECT:usrNum EXCLUSIVE-LOCK NO-ERROR. //Order-Line.Order-num = tt-OrderLine.tt-OrderNum AND 
                
                IF NOT AVAILABLE Users THEN
                DO:
                    CREATE Users.
                    ASSIGN
                        Users.usrid     = ttUser.ttusrid  
                        Users.firstname = ttUser.ttfirstName   
                        Users.lastname  = ttUser.ttlastName 
                        Users.emailid   = ttUser.ttemaiid
                        Users.phone     = ttUser.ttphone. 
                /*                    MESSAGE "added"       */
                /*                        VIEW-AS ALERT-BOX.*/
                        
                END.
                ELSE 
                DO:
                    UNDO,THROW NEW Progress.Lang.AppError("unable to add user",1).
                END.
            END. 
        END.       
        RETURN.

        //CATCH e AS Progress.Lang.Error:
        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID Advancefilter( OUTPUT TABLE ttUser ):
        FOR EACH Users WHERE Users.firstname BEGINS THIS-OBJECT:cFirstName 
            AND Users.lastname BEGINS THIS-OBJECT:clastName AND 
            Users.phone BEGINS THIS-OBJECT:cPhone AND Users.emailid BEGINS THIS-OBJECT:cEmail  use-index idx_usr NO-LOCK:
            CREATE ttUser.
            ASSIGN
                ttUser.ttusrid     = Users.usrid
                ttUser.ttfirstName = Users.firstname
                ttUser.ttlastName  = Users.lastname
                ttUser.ttphone     = Users.phone
                ttUser.ttemaiid    = Users.emailid.
        END. 
        IF NOT AVAILABLE ttUser THEN
        DO:
            UNDO,THROW NEW Progress.Lang.AppError("invalid ",1).
                         
        END.
        
        RETURN.

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).


        END CATCH.
        
        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID getUserDetails( INPUT cfirstname as Character ,INPUT clastname AS CHARACTER ,INPUT cPhone as CHARACTER,INPUT cemail AS CHARACTER ,OUTPUT TABLE ttUser):

        DEFINE VARIABLE cWhereClause AS CHARACTER NO-UNDO.
        DEFINE VARIABLE qh           AS HANDLE.

        FIND FIRST Users WHERE Users.firstname = cfirstname OR Users.lastname = clastname OR Users.phone = cPhone OR Users.emailid = cemail NO-LOCK NO-ERROR.
        IF AVAILABLE Users THEN
        DO:
            IF cfirstname <> "" THEN
            DO:
                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " firstname = " + quoter(cfirstname).

            END.
            IF clastname <> "" THEN
            DO:

                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " lastname = " + quoter(clastname).

            END.
            IF cphone <> "" THEN
            DO:

                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " phone = " + quoter(cphone).

            END.
            IF cemail <> "" THEN
            DO:

                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " emailid = " + quoter(cemail).

            END.
        END.
        ELSE
        DO:
            MESSAGE "ENTER AVAILABLE RECORD..."
                VIEW-AS ALERT-BOX.

        END.

        IF(cWhereClause = "") THEN
        DO:
        END.
        ELSE
        DO:

            CREATE QUERY qh.
            qh:SET-BUFFERS(BUFFER Users:HANDLE).
            qh:QUERY-PREPARE ("FOR EACH " +  "Users" + cWhereClause).


            DO:
                qh:QUERY-OPEN.
                DO WHILE  qh:GET-NEXT():
                    CREATE ttUser.
                    ASSIGN 
                        ttUser.ttusrid     = Users.usrid
                        ttUser.ttfirstName = Users.firstname
                        ttUser.ttlastName  = Users.lastname
                        ttUser.ttemaiid    = Users.emailid
                        ttUser.ttphone     = Users.phone.
                END.


                qh:QUERY-CLOSE().
                DELETE OBJECT qh.
            END.
        END.
        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL deleteUser(  ):
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        FIND FIRST Users WHERE Users.usrid = THIS-OBJECT:usrNum EXCLUSIVE-LOCK NO-ERROR.
                
        IF AVAILABLE Users THEN
        del_UserTicket:
        DO TRANSACTION
            ON ERROR UNDO del_UserTicket,LEAVE del_UserTicket:
            DO:
                DELETE Users.
                lResult = TRUE.
                MESSAGE "sucess"
                    VIEW-AS ALERT-BOX.
            END.
            
            
        END.
        RETURN lResult.  

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL deleteUserTicket(  ):
        // METHOD PUBLIC LOGICAL getDelOrderLine(  ):
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
       // FIND FIRST UserTicket WHERE UserTicket.UserTicketID = THIS-OBJECT:iusrTicketId EXCLUSIVE-LOCK NO-ERROR.
        for each UserTicket WHERE UserTicket.UserTicketID = THIS-OBJECT:iusrTicketId EXCLUSIVE-LOCK:
                
            IF AVAILABLE UserTicket THEN
            del_UserTicket:
            DO TRANSACTION
                ON ERROR UNDO del_UserTicket,LEAVE del_UserTicket:
                DO:
                    DELETE UserTicket.
                    lResult = TRUE.
                    MESSAGE "sucess"
                        VIEW-AS ALERT-BOX.
                END.
        
            
            END.
        end.
        RETURN lResult.

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID reportGen(  ):
        
        output to "itrequest.csv".
        EXPORT DELIMITER "," "Firstname" "LastName" "usrid" "ticketsatus" "startdate" .

        for each UserTicket where UserTicket.startdate = today use-index idx_usrticketstartdate no-lock:
            if UserTicket.ticketstatus = "open" then
            DO:
                FOR EACH Users WHERE Users.usrid = UserTicket.usrid:

                    EXPORT DELIMITER ","  Users.firstname Users.lastname  
                        UserTicket.usrid   UserTicket.ticketstatus  UserTicket.startdate.
                END.
            END.
        END.

        MESSAGE "generated"
            VIEW-AS ALERT-BOX.
        /*     end.*/

        output close.
        RETURN.
                
                
                
                
                
        /*        DEFINE VARIABLE assigneename AS CHARACTER NO-UNDO.                                                                      */
        /*                                                                                                                                */
        /*        OUTPUT TO "exmp.text".                                                                                                  */
        /*                                                                                                                                */
        /*        EXPORT DELIMITER "," "User Id"  "Ticket ID" "Start Dat " "Assignee Name" "Ticket Status"  .                             */
        /*                                                                                                                                */
        /*        FOR EACH userticket NO-LOCK                                                                                             */
        /*            WHERE userticket.startdate = TODAY:                                                                                 */
        /*                                                                                                                                */
        /*            FIND FIRST ticketassignee   NO-LOCK                                                                                 */
        /*                WHERE ticketassignee.CODE =    userticket.assignee  NO-ERROR.                                                   */
        /*            IF AVAILABLE ticketassignee THEN                                                                                    */
        /*            DO:                                                                                                                 */
        /*                assigneename =  ticketassignee.NAME.                                                                            */
        /*            END.                                                                                                                */
        /*                                                                                                                                */
        /*            EXPORT DELIMITER "," userticket.usrId userticket.ticketid userticket.startdate assigneename userticket.ticketstatus.*/
        /*        END.                                                                                                                    */
        /*                                                                                                                                */
        /*        OUTPUT CLOSE.                                                                                                           */
        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID updateUser(INPUT TABLE ttUser  ):
        FIND FIRST ttUser WHERE ttUser.ttusrid  = THIS-OBJECT:usrNum NO-LOCK NO-ERROR. //check the temp table 
        IF AVAILABLE ttUser THEN 
        DO:
            /*            MESSAGE "entry cls"   */
            /*                VIEW-AS ALERT-BOX.*/
            update_userTicket:                                       
            DO TRANSACTION
                ON ERROR UNDO update_userTicket , LEAVE update_userTicket:
                FIND FIRST Users WHERE Users.usrid = ttUser.ttusrid EXCLUSIVE-LOCK NO-ERROR. //its find the order in database
                IF AVAILABLE Users THEN
                DO:
                 
                    ASSIGN
                        
                        Users.usrid     = ttUser.ttusrid
                        Users.firstname = ttUser.ttfirstName
                        Users.lastname  = ttUser.ttlastName
                        Users.phone     = ttUser.ttphone
                        Users.emailid   = ttUser.ttemaiid.
                                        
                    MESSAGE ttUser.ttusrid
                        VIEW-AS ALERT-BOX.
                    
                END.  
                ELSE
                DO:
                    UNDO,THROW NEW Progress.Lang.AppError("not updated ",1).
                         
                END.
            END.
        END. 
        RETURN.

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID UpdateUsrTicket( INPUT TABLE ttuserTicket ):
        FOR EACH ttuserTicket NO-LOCK: // WHERE ttuserTicket.tusrTicketId  = THIS-OBJECT:iusrTicketId NO-LOCK: // NO-ERROR. //check the temp table 
            IF AVAILABLE ttuserTicket THEN 
            DO:
                /*            MESSAGE "entry cls"   */
                /*                VIEW-AS ALERT-BOX.*/
                update_userTicket:                                       
                DO TRANSACTION
                    ON ERROR UNDO update_userTicket , LEAVE update_userTicket:
                    FIND FIRST UserTicket WHERE UserTicket.UserTicketID = ttuserTicket.tusrTicketId EXCLUSIVE-LOCK NO-ERROR.
                   // FIND CURRENT UserTicket. //its find the order in database
                    IF AVAILABLE UserTicket THEN
                    DO:
                        
                    
                        IF ttuserTicket.tticketStatus = "Close" THEN
                        DO:
                            ASSIGN
                        
                                UserTicket.UserTicketID = ttuserTicket.tusrTicketId
                                
                                UserTicket.enddate      = TODAY 
                                UserTicket.ticketstatus = ttuserTicket.tticketStatus.
                        END.
                   // END.
                        ELSE
                        DO:
                            ASSIGN
                                UserTicket.enddate      = DATE("")
                                UserTicket.ticketstatus = ttuserTicket.tticketStatus.
                        END.
                        IF ttuserTicket.tenddate >= ttuserTicket.tstartdate THEN
                        DO:
                            ASSIGN

                                UserTicket.UserTicketID = ttuserTicket.tusrTicketId
                                UserTicket.enddate      = ttuserTicket.tenddate
                                UserTicket.ticketstatus = ttuserTicket.tticketStatus.
                        END.
                   // END.
                        ELSE
                        DO:
                        /*                            MESSAGE "invalid"     */
                        /*                                VIEW-AS ALERT-BOX.*/
                        END.

                       
                    
                    END.  
                    ELSE
                    DO:
                        UNDO,THROW NEW Progress.Lang.AppError("not updated ",1).
                         
                    END.
                END.
            END.
        /*        END.*/
        END.
        
        
        RETURN.

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        END CATCH.


        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID userFetch( INPUT hbufhandle AS HANDLE ):
        
        
        FIND FIRST users 
            WHERE users.usrid = THIS-OBJECT:usrNum NO-LOCK NO-ERROR.
        IF AVAILABLE users THEN
        DO:
            
            hbufhandle:BUFFER-CREATE().
            ASSIGN 
                hbufhandle::ttusrid     = users.usrid
                hbufhandle::ttfirstName = users.firstname
                hbufhandle::ttlastName  = users.lastname
                hbufhandle::ttemaiid    = users.emailid
                hbufhandle::ttphone     = users.phone.
                
        END.
        ELSE
        DO:
            UNDO,THROW NEW Progress.Lang.AppError("invalid ID",1).
        END.
        RETURN.

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID userTicketfetch( INPUT hbuffer AS HANDLE ):
        
        FOR EACH  UserTicket WHERE UserTicket.usrId = THIS-OBJECT:usrNum NO-LOCK:
            
            FIND FIRST Ticket WHERE Ticket.ticketid = UserTicket.ticketid NO-LOCK NO-ERROR.

            IF AVAILABLE Ticket THEN
            DO:
                FIND FIRST TicketAssignee WHERE TicketAssignee.code = UserTicket.assignee NO-LOCK NO-ERROR.
                IF AVAILABLE TicketAssignee THEN
                DO:
               
                    hbuffer:BUFFER-CREATE ().
                    ASSIGN
                        hbuffer::tusrid             = UserTicket.usrid
                        hbuffer::tusrTicketId       = UserTicket.UserTicketID
                        hbuffer::tticketId          = UserTicket.ticketid
                        hbuffer::tstartdate         = UserTicket.startdate
                        hbuffer::tTicketDiscription = Ticket.ticketdescription
                        hbuffer::tenddate           = UserTicket.enddate
                        hbuffer::tassigne           = TicketAssignee.name
                        hbuffer::tcategory          = Ticket.category
                        hbuffer::tticketStatus      = UserTicket.ticketstatus.
                        
                END.
               
            END.
            
        END.         
        RETURN.

        CATCH e AS Progress.Lang.Error:
            UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC ItRequest ( ):

    END DESTRUCTOR.

END CLASS.