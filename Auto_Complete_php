# ac-php   [![MELPA](http://melpa.org/packages/ac-php-badge.svg)](http://melpa.org/#/ac-php)
emacs auto-complete for php


use [phpctags](https://github.com/xcwen/phpctags) gen tags 

and use `ac-php`  work with tags 

 
## Table of Contents


* [Test](#test)
* [Usage](#usage)
* [php extern for complete](#php-extern-for-complete)
* [rebuild tags](#rebuild-tags)
* [FQA](#fqa)


##  Test
* install `php5-cli` command  for phpctags
```bash 
\#UBUNTU
localhost:~/$ sudo apt-get install php5-cli 
```
* install `cscope` command  for `ac-php-cscope-find-egrep-pattern`
```bash 
\#UBUNTU
localhost:~/$ sudo apt-get install cscope
```

* example:
![example.gif](https://raw.githubusercontent.com/xcwen/ac-php/master/images/ac-php.gif)

```bash
\#backup old .emacs
cp ~/.emacs ~/.emacs.bak
```

save it as `~/.emacs`
```elisp
  (setq package-archives
        '(("melpa" . "http://melpa.milkbox.net/packages/")) )
  (package-initialize)
  (unless (package-installed-p 'ac-php )
    (package-refresh-contents)
    (package-install 'ac-php )
    )
  (require 'cl)
  (require 'php-mode)
  (add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
               (setq ac-sources  '(ac-source-php ) )
               (yas-global-mode 1)
               (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
               ))
```

```bash
cd ~/
git clone https://github.com/xcwen/ac-php/
\#test php files in ~/ac-php/phptest
\#open file for test
emacs ~/ac-php/phptest/testb.php
```

# Usage

* install `php5-cli` command  for phpctags
```bash 
\#UBUNTU
localhost:~/$ sudo apt-get install php5-cli 
```
* install `cscope` command  for `ac-php-cscope-find-egrep-pattern`
```bash 
\#UBUNTU
localhost:~/$ sudo apt-get install cscope
```



* install `ac-php` from melpa
```elisp
  (setq package-archives
        '(("melpa" . "http://melpa.milkbox.net/packages/")) )
```

"M-x" :`package-list-packages`  find  `ac-php` install

* emacs php-mode function  define

```elisp
  (add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
               (setq ac-sources  '(ac-source-php ) )
               (yas-global-mode 1)
               (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
               ))
```

`ac-php-remake-tags` ;; **if source is changed ,re run this commond for update tags**

`ac-php-show-tip` ;; show define at point

`ac-php-cscope-find-egrep-pattern` ;; find current-word in project 



* mkdir ".tags"  in root of project

``` bash
cd /project/to/path #  root dir of project
mkdir .tags
```
* DONE 


## Php Doc for complete  
define class memeber type :

`public  $v1;`  =>
``` php
/**
 * @var classtype
 */
public $v1;
```

define class function   return type:

`public  function get_v1()`  =>
```php
/**
 * @return classtype 
 */
public function get_v1()
```

define variable: 

`$value=new Testa ();` => 
```php
/** @var $value  Testa */
$value=new Testa();
```

like this
```php
class Testa {
    /**
     * @var Testb; 
     */
	public  $v1;
    /**
     * @var \Test\TestC; 
     */
	public  $v2;
    public function set_v1($v){
        $v=trim($v);
        //define value type
        /** @var $v Testb*/
        $v=new Testb();
        $this->v1=$v;
    }
    /**
     * 
     * @return  \Test\TestC; 
     */
    public function get_v1(){
        $this->get_v1("ss");
    }
}
```


## Rebuild Tags
tags file location dir is in  `.tags`   for example:  `/project/to/path/.tags`
```bash
localhost:~/ac-php/phptest/.tags$ tree .
.
├── tags-data.el
└── tags_dir_jim
    ├── a.tags
    ├── testa.tags
    └── testb.tags
1 directory, 4 files
```



**if source is changed ,re run this commond for update tags**: `ac-php-remake-tags` 

if php file cannot pass from `phpctags`.

you can find any  error from `Messages` buffer  fix it and next

like this 
```
phpctags[/home/jim/phptest/testa.php] ERROR:PHPParser: Unexpected token '}' on line 11 - 
```
you need fix testa.php  error and re run `ac-php-remake-tags`


if show:
```
no find .tags dir in path list :/home/jim/phptest/ 
```

mk .tags dir  like this:

`mkdir /home/jim/phptest/.tags`

or

`mkdir /home/jim/.tags `

or

`mkdir /home/.tags`


## FQA
