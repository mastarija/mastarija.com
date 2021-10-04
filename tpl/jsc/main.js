! function ()
{
  let link = window.location.pathname
  let menu = Array
    .from( document.querySelectorAll( '#main-menu li a' ) )
    .forEach( x =>
        {
          let href = x.getAttribute( 'href' )

          if ( link == href || link.startsWith( href ) && ( href.length > 1 ) )
            x.parentNode.classList.add( 'active' )
        } )
} ()
