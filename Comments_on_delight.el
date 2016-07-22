Why delete this page? MELPA still fetches its delight.el package's source from here.

-- [https://github.com/dalymatthew mdaly] 2016-07-21 13:59 UTC


----

Dunno why PhilS deleted it. I rolled it back, for now. I'm guessing that Phil had a good reason and can mention that here. It is his library, after all.

-- DrewAdams 2016-07-21 14:36 UTC


----

I'm guessing he wants to host it from GitHub. Here's a copy I found: https://github.com/phil-s/emacsd/tree/53afb004856c2eaabe86543af7fea196dd04cbc1/el-get/delight

-- [http://alexschroeder.ch/wiki/Emacs Alex Schroeder] 2016-07-21 14:39 UTC


----

OK. In that case, he probably needs to update the recipe for MELPA, to pull from there instead of here. At that point it would probably be good to delete the copy here.

-- DrewAdams 2016-07-21 17:38 UTC


----

Ah, discussion. Actually, I never put delight.el on MELPA at all -- someone else did that. I don't know how to undo it, and I shan't be maintaining this library on the Wiki any more, so MELPA's version would end up out of date. Better that people get the package from GNU ELPA than continue to unwittingly use an out-of-date version (even if right now both of them have v1.5). I suppose MELPA could get it from Savannah, but I'm pretty sure it would be better for MELPA to drop it completely? Unless someone has actually removed GNU ELPA from their package archive list, package.el would surely still find it when people go to install it.

-- Phil S 2016-07-22 09:26 UTC


----

I've updated https://www.emacswiki.org/emacs/DelightedModes with a direct link to the code in its new repository.

-- Phil S 2016-07-22 09:28 UTC


----

I've raised https://github.com/melpa/melpa/issues/4059 as well.

-- Phil S 2016-07-22 09:43 UTC

