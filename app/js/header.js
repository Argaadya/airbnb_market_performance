export function togglePeriodBox(message) {
  console.log('togglePeriodBox triggered');

  const element = document.getElementById('listing_date');
  const button = document.getElementById('date_button');

  if (!element || !button) {
    console.error('Element or button not found');
    return;
  }

  if (!element || !button) return; // Ensure elements exist

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
