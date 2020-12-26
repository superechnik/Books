import Button from '@material-ui/core/Button';
import Toolbar from '@material-ui/core/Toolbar';
import IconButton from '@material-ui/core/IconButton';
import Typography from '@material-ui/core/Typography';
import MenuIcon from '@material-ui/core/MenuItem';
import { makeStyles } from '@material-ui/core/styles';
import AppBar from '@material-ui/core/AppBar';

const useStyles = makeStyles((theme) => ({
    root: {
      flexGrow: 1,
    },
    menuButton: {
      marginRight: theme.spacing(2),
    },
    toolbar: {
      minHeight: 128,
      alignItems: 'flex-start',
      paddingTop: theme.spacing(1),
      paddingBottom: theme.spacing(2),
    },
    title: {
      flexGrow: 1,
      alignSelf: 'flex-end',
    },
  }));
  

function  NavBar() {

    const classes = useStyles();
    
    return (
        <AppBar position="static">
        <Toolbar variant="dense">
          <IconButton edge="start" className={classes.menuButton} color="inherit" aria-label="menu">
            <MenuIcon />
          </IconButton>
          <Typography variant="h6" color="inherit">
            Navigation
          </Typography>
        </Toolbar>
      </AppBar>
    );}

export default NavBar;