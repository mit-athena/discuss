main()
{
	execlp("discuss", "discuss", "-prompt", 
	       "discuss_list_meetings", "-request", "list_meetings", 
	       "-quit", 0);
	perror("discuss");
}
