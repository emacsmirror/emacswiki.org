// Tags - make a tags table by reflection
// (C) Tapsell-Ferrier Limited 2004

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.regex.*;
import java.util.jar.*;
import java.util.zip.*;


/** Make a tags table from the class files in a classpath.
 * The classpath is obtained from the property <code>java.class.path</code>
 *
 * The class names that get output must match the <code>packageFilter</code>
 * if it is specified.
 *
 * @author Nic Ferrier <nferrier@tapsellferrier.co.uk>
 */
public class Tags
{

  private PrintWriter out = new PrintWriter(System.out, true);


  /** If this is not null it's used as a filter for acceptable classes.
   * Only packages that <code>matches(packageFilter)</code> will be tagged.
   */
  protected String packageFilter;
  

  private void processClasspath (String classpath)
    throws PatternSyntaxException
  {
    StringTokenizer st = new StringTokenizer(classpath, ":");
    while (st.hasMoreTokens())
      {
        String element = st.nextToken();
        File f = new File(element);
        if (f.exists())
          if (f.isDirectory())
            processDirectory(f);
          else
            processJarFile(f);
      }
  }

  private void processJarFile (File f)
    throws PatternSyntaxException
  {
    try
      {
        JarFile jar = new JarFile(f);
        Enumeration en = jar.entries();
        int i = 0;
        while (en.hasMoreElements())
          {
            ZipEntry z = (ZipEntry) en.nextElement();
            String name = z.getName();

            if (name.indexOf(".class") < 0)
              continue;

            String className = name.substring(0, name.lastIndexOf(".class")).replace('/', '.');
            if (packageFilter != null && !(className.matches(packageFilter)))
              continue;

            try
              {
                Class c = Class.forName(className);
                tagClass(c);
              }
            catch (ClassNotFoundException e)
              {
                System.err.println("Class not found:" + className);
              }
            catch (NoClassDefFoundError e)
              {
                System.err.println("Class not found:" + className);
              }
            catch (UnsatisfiedLinkError e)
              {
                System.err.println("Class's linkage failed:" + className);
              }
          }
      }
    catch (IOException e)
      {
        System.err.println("Bad jar?");
      }
  }


  /** Find packages in the directory.
   */
  private void processDirectory (File d)
  {
    /*
      File[] packages = d.listFiles();
    for (int i = 0; i < packages.length; i++)
      {
        
      }
    */
  }
  
  private void tagClass(Class c)
  {
    out.println(c.getName());
    tagConstructors(c);
    tagMethods(c);
  }

  private void tagConstructors(Class c)
  {
    try
      {
        Constructor[] methods = c.getDeclaredConstructors();
        for (int i = 0; i < methods.length; i++)
          try
            {
              String mods = getModifiers(methods[i].getModifiers());
              out.print("\t" + mods);
              out.print(methods[i].getName() + "(");

              Class[] params = methods[i].getParameterTypes();
              writeParameters(params);

              out.println();
            }
          catch (IllegalStateException e)
            {
              // Throw this away because it means the constructor was private
            }
      }
    catch (NoClassDefFoundError e)
      {
        System.err.println(c.getName() + " is not found.");
      }
    catch (Exception e)
      {
        System.err.println(e.getMessage());
      }
  }
  
  private void tagMethods(Class c)
  {
    try
      {
        Method[] methods = c.getDeclaredMethods();
        for (int i = 0; i < methods.length; i++)
          try
            {
              String mods = getModifiers(methods[i].getModifiers());
              out.print("\t" + mods);
              out.print(c.getName() + "." + methods[i].getName() + "(");

              Class[] params = methods[i].getParameterTypes();
              writeParameters(params);

              // Return type
              String returnType = normalizeParameterName(methods[i].getReturnType().getName());
              out.print(" returns " + returnType);

              Class[] exceptions = methods[i].getExceptionTypes();
              writeExceptions(exceptions);

              out.println();
            }
          catch (IllegalStateException e)
            {
              // Throw this away because it means the constructor was private
            }
      }
    catch (NoClassDefFoundError e)
      {
        System.err.println(c.getName() + " is not found.");
      }
    catch (Exception e)
      {
        System.err.println(e.getMessage());
      }
  }

  // throws an exception when the access is not public or protected
  private String getModifiers (int mod)
  {
    StringBuffer mods = new StringBuffer();
    if (Modifier.isPublic(mod))
      mods.append("public ");
    else if (Modifier.isProtected(mod))
      mods.append("protected ");
    else
      throw new IllegalStateException("private or package private");

    if (Modifier.isAbstract(mod))
      mods.append("abstract ");
    if (Modifier.isFinal(mod))
      mods.append("final ");
    if (Modifier.isStatic(mod))
      mods.append("static ");

    return mods.toString();
  }
 
  private void writeParameters(Class[] params)
  {
    if (params.length < 1)
      out.print(")");
    else
      for (int k = 0; k < params.length; k++)
        {
          String paramName = normalizeParameterName(params[k].getName());
          out.print(paramName + ((k < params.length - 1) ? ", " : ")"));
        }
  }

  private void writeExceptions(Class[] exceptions)
  {
    if (exceptions.length > 0)
      {
        out.print(" throws ");
        for (int i = 0; i < exceptions.length; i++)
          {
            String exceptionName = exceptions[i].getName();
            out.print(exceptionName + ((i < exceptions.length - 1) ? ", " : ""));
          }
      }
  }

  // Ensures array types are translated to something readable
  private static String normalizeParameterName(String paramName)
  {
    if (paramName.startsWith("[L"))
      return paramName.substring(2, paramName.length() - 1) + "[]";
    return paramName;
  }
  
  public static void main (String[] argv) throws Exception
  {
    Tags tags = new Tags();

    if (argv.length > 0)
      tags.packageFilter = argv[0];

    tags.processClasspath(System.getProperty("java.class.path"));

    System.exit(0);
  }
}
