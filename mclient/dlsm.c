main()
{
	execlp("discuss", "discuss", "-subsystem_name", 
	       "discuss_list_meetings", "-request", "list_meetings", 
	       "-quit", 0);
	perror("discuss");
}
