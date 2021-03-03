# Books

This project has been under development for many years using different languages and frameworks.
I decided to restart it with a Haskell backend because I love Haskell.
I chose React with Typescript for the UI because I'm a backend developer and wanted a challenge.

The ultimate purpose of this project is to catalog all the physical books I own.

Cool feature idea:  I want to be able to read an ISBN from the camera.

**Current State 2021:** Get/Put/Post/Delete functioning using curl. UI only has a get and a grid. The Haskell plug in
for VS Code is broken/doesn't work on windows so I'll have to transfer development to a Linux box which is not 
always available.

If you really wanted to run this, download Haskell and start the Backend folder with `stack run`, wait
for it to compile (maybe get a coffee), come back and cd to Frontend and `npm start`.  

### Disclaimer! 
The Haskell backend code was copied from a blog post somewhere because I didn't have the bandwidth to figure 
out the *Snap* library at the time.  The code as posted was non-functioning. I fixed it, upgraded the packages and 
fixed it again.
