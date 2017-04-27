$(document).ready(function(){
  var list = $('#projects');
  var disclaimer = $('.disclaimerText');
  $('#projectList').on('click', function(){
    toggleRotate(list, this);
  });
  $('.disclaimer').on('click', function(){
    toggleRotate(disclaimer, this);
  });
  function toggleRotate(target, rotate){
    $(target).toggle('slow');
    $(rotate).find('.arrow svg').toggleClass('rotate');
  }
});

