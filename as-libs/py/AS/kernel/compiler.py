import hashlib
import time
import linecache

from IPython.core.compilerop import CachingCompiler

#-----------------------------------------------------------------------------
# Local utilities
#-----------------------------------------------------------------------------

def code_name(code, number=0):
    """ Compute a (probably) unique name for code for caching.
    
    Expects code to be a unicode string.
    """
    hash_digest = hashlib.md5(code.encode("utf-8")).hexdigest()
    # Include the number and 12 characters of the hash in the name.  It's
    # pretty much impossible that in a single session we'll have collisions
    # even with truncated hashes, and the full one makes tracebacks too long
    return '<alphasheets-input-{0}-{1}>'.format(number, hash_digest[:12])

#-----------------------------------------------------------------------------
# Compiler
#-----------------------------------------------------------------------------

class ASCompiler(CachingCompiler):
  def cache(self, code, number=0):
        """Make a name for a block of code, and cache the code.
        
        Parameters
        ----------
        code : str
          The Python source code to cache.
        number : int
          A number which forms part of the code's name. Used for the execution
          counter.
          
        Returns
        -------
        The name of the cached code (as a string). Pass this as the filename
        argument to compilation, so that tracebacks are correctly hooked up.
        """
        name = code_name(code, number)
        entry = (len(code), time.time(),
                 [line+'\n' for line in code.splitlines()], name)
        linecache.cache[name] = entry
        linecache._ipython_cache[name] = entry
        return name