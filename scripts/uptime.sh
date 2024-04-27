uptime | awk -F'( |,|:)+' '{
    d=h=m=0;
    if ($7=="min")
        m=$6;
    else {
        if ($7~/^day/) { d=$6; h=$8; m=$9}
        else {h=$6;m=$7}
        }
    }
    {
        printf("up %d days %d hours %d minutes",d+0,h+0,m+0)
    }'
