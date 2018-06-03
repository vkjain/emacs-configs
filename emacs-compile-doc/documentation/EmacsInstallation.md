## Building Emacs on macOS Sierra
As usual, you need Xcode ([free in the Mac App Store][Xcode]) to build. But, you already have that, right?
You also need [Autoconf](https://www.gnu.org/software/autoconf/) and [Automake](http://www.gnu.org/software/automake/) The simple approach is to get them from Homebrew:

#### Installing Autoconf
```sh
cd /tmp
curl -O http://ftp.gnu.org/gnu/autoconf/autoconf-latest.tar.gz
tar xf autoconf-latest.tar.gz
cd autoconf-*/
./configure
make
sudo make install
```
#### Installing Automake
```sh
cd /tmp
curl -O http://ftp.gnu.org/gnu/automake/automake-1.15.tar.gz
tar xf automake-1.15.tar.gz
cd automake-1.15
./configure
make
sudo make install
```
Next grab the source
```sh
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
```
And checkout the emacs-26 branch (master is the development branch):
Note that you can get list of all remote branches in the emacs git repository
by typing 
```sh
git branch -r --list
```
We wish to checkout branch 'emacs-26'
```sh
git checkout emacs-26
```
![emacs-26 branch](emacs1.png)

Time to build:
```sh
make configure
./configure --with-ns
make install
```

configure file created

![configure file built](emacs%20configure.png)

And don’t panic, make install build the application bundle, it doesn’t actually install anything. When make finishes, the app will have build as nextstep/Emacs.app. To take it for a test drive:
Note that nextstep is actually inside the emacs-26 folder on the local system.

```sh
open nextstep/Emacs.app
```

If it looks good, you can install it by revealing it in the Finder. So the next command reveals Emacs.app in the Finder.

```sh
open -R nextstep/Emacs.app
```

and dragging Emacs to the Applications folder.

Release 26 is definitely a refinement release. 




[Xcode]:(https://itunes.apple.com/us/app/xcode/id497799835)


