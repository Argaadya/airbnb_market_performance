document.addEventListener('DOMContentLoaded', () => {
  const dragHandle = document.getElementById('drag-handle');
  const sidebar = document.getElementById('sidebar');
  const main = document.getElementById('main');

  if (!dragHandle || !sidebar || !main) return; // Ensure elements exist

  dragHandle.addEventListener('mousedown', (e) => {
    function onMouseMove(e) {
      const newWidth = e.pageX;
      const maxWidth = window.innerWidth;

      if (newWidth >= 200 && newWidth <= maxWidth) {
        sidebar.style.width = `${newWidth}px`;
        main.style.width = `${maxWidth - newWidth}px`; // Adjust main width
      }
    }

    function onMouseUp() {
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
    }

    document.addEventListener('mousemove', onMouseMove);
    document.addEventListener('mouseup', onMouseUp);
  });
});


export function uiDateConvert(date) {
  if (!date) return '';

  const year = date.getFullYear();
  const month = (`0${date.getMonth() + 1}`).slice(-2);
  const day = (`0${date.getDate()}`).slice(-2);

  return `${year}-${month}-${day}`;
}




Shiny.addCustomMessageHandler('togglePeriodBox', function(message) {
    const element = document.getElementById('listing_date');
    const button = document.getElementById('date_button');

    if (element) {
        if (element.classList.contains('fade-in')) {
            element.classList.remove('fade-in');
            setTimeout(() => { element.style.display = 'none'; }, 500);
            button.style.backgroundColor = '';
            button.style.color = '';
        } else {
            element.style.display = 'block';
            setTimeout(() => { element.classList.add('fade-in'); }, 100);
            button.style.backgroundColor = '#4B77BE';
            button.style.color = 'white';
        }
    }
});


Shiny.addCustomMessageHandler('toggleListingBox', function(message) {
    const element = document.getElementById('listing_box');
    const button = document.getElementById('listing_button');

    if (element) {
        if (element.classList.contains('fade-in')) {
            element.classList.remove('fade-in');
            setTimeout(() => { element.style.display = 'none'; }, 500);
            button.style.backgroundColor = '';
            button.style.color = '';
        } else {
            element.style.display = 'block';
            setTimeout(() => { element.classList.add('fade-in'); }, 100);
            button.style.backgroundColor = '#4B77BE';
            button.style.color = 'white';
        }
    }
});

Shiny.addCustomMessageHandler('toggleAmenitiesBox', function(message) {
    const element = document.getElementById('listing_amenities');
    const button = document.getElementById('amenities_button');

    if (element) {
        if (element.classList.contains('fade-in')) {
            element.classList.remove('fade-in');
            setTimeout(() => { element.style.display = 'none'; }, 500);
            button.style.backgroundColor = '';
            button.style.color = '';
        } else {
            element.style.display = 'block';
            setTimeout(() => { element.classList.add('fade-in'); }, 100);
            button.style.backgroundColor = '#4B77BE';
            button.style.color = 'white';
        }
    }
});